(import (scheme base)
  (liii check)
  (liii list)
  (liii string)
  (liii path)
  (liii os)
  (liii path stat)
  (scheme time)
  (liii json)
  (liii pi code modes config state)
  (liii pi code modes config layout)
  (liii pi code modes config files)
  (liii pi code modes config json-write)
) ;import

(check-set-mode! 'report-failed)

;; 配置 TUI 的文件层：
;;   pi-config-file-view    只读视图（路径/是否存在/原文）
;;   pi-config-plan-toggle  空格启停的写入计划（纯函数）
;;   pi-config-apply-writes 原子落盘

(define FIXTURE
  (if (file-exists? "tools/pi/tests/fixtures/user-config")
    "tools/pi/tests/fixtures/user-config"
    "tests/fixtures/user-config"))

(define (ref alist key)
  (let ((pair (assoc key alist))) (and pair (cdr pair)))
) ;define

;; writes 是 ((相对路径 . 文本) ...)：取某个文件将要写入的文本
(define (p-index st id)
  (list-index (lambda (p) (string=? (pi-config-provider-id p) id))
    (pi-config-state-providers st)
  ) ;list-index
) ;define

(define (wtext plan rel)
  (let ((pair (assoc rel (ref plan "writes")))) (and pair (cdr pair)))
) ;define

;; 每个用例在独立的临时目录副本上跑，避免相互污染
(define %dir-counter 0)

(define (make-dir name)
  (set! %dir-counter (+ %dir-counter 1))
  (let* ((src (path-join (path FIXTURE) (path "config-tui")))
         (dst (path-join (path-temp-dir)
                (string-append "pi-config-files-" name "-"
                  (number->string (current-second))
                  "-"
                  (number->string %dir-counter)
                ) ;string-append
              ) ;path-join
         ) ;dst
        ) ;
    (define (copy-tree s d)
      (unless (path-exists? d) (path-mkdir d :parents #t :exist-ok #t))
      (for-each
        (lambda (name)
          (let ((sp (path-join s (path name))) (dp (path-join d (path name))))
            (if (path-stat-dir? (path-stat (path->string sp)))
              (copy-tree sp dp)
              (path-copy sp dp)
            ) ;if
          ) ;let
        ) ;lambda
        (vector->list (listdir (path->string s)))
      ) ;for-each
    ) ;define
    (copy-tree src dst)
    dst
  ) ;let*
) ;define

;; ---------- 只读文件视图 ----------

;; 1. provider 有配置文件：路径 + 原文
(let* ((dir (make-dir "view-provider"))
       (st (pi-config-state (path->string dir) :env '()))
       (v (pi-config-file-view st (pi-config-cursor (p-index st "kimi-coding") 0) 0))
      ) ;
  (check (ref v "kind") => "provider")
  (check (ref v "path") => "models/kimi-coding.json")
  (check (ref v "exists") => #t)
  (check (string-contains (ref v "content") "Kimi Coding") => #t)
) ;let*

;; 2. provider 虚拟条目（无文件）：exists? 为假，content 为 #f，路径仍给出
(let* ((dir (make-dir "view-virtual"))
       (st (pi-config-state (path->string dir) :env '()))
       (v (pi-config-file-view st (pi-config-cursor (p-index st "google") 0) 0))
      ) ;
  (check (ref v "path") => "models/google.json")
  (check (ref v "exists") => #f)
  (check (ref v "content") => #f)
) ;let*

;; 3. 模型有配置文件
(let* ((dir (make-dir "view-model"))
       (st (pi-config-state (path->string dir) :env '()))
       (v (pi-config-file-view st (pi-config-cursor (p-index st "kimi-coding") 0) 1))
      ) ;
  (check (ref v "kind") => "model")
  (check (ref v "path") => "models/kimi-coding/k3.1-pro.json")
  (check (ref v "exists") => #t)
  (check (string-contains (ref v "content") "Kimi K3.1 Pro") => #t)
) ;let*

;; 4. 只读：取视图不改变文件
(let* ((dir (make-dir "view-readonly"))
       (target (path->string (path-join dir (path "models") (path "kimi-coding.json"))))
       (before (path-read-text (path target)))
       (st (pi-config-state (path->string dir) :env '()))
      ) ;
  (pi-config-file-view st (pi-config-cursor (p-index st "kimi-coding") 0) 0)
  (check (path-read-text (path target)) => before)
) ;let*

;; ---------- 空格启停：计划 ----------

;; 5. 禁用 provider：从白名单移除
(let* ((dir (make-dir "toggle-provider-off"))
       (st (pi-config-state (path->string dir) :env '()))
       (plan (pi-config-plan-toggle st (pi-config-cursor (p-index st "kimi-coding") 0) 0))
       (text (wtext plan "models/providers.json"))
      ) ;
  (check (ref plan "ok") => #t)
  (check (string-contains text "google") => #t)
  (check (string-contains text "kimi-coding") => #f)
  (check (string-contains text "custom-kimi") => #t)
) ;let*

;; 6. 禁用最后一个 provider：拒绝
(let* ((dir (make-dir "toggle-last-provider"))
       (st (pi-config-state (path->string dir) :env '()))
       (disabled (map pi-config-provider-id
                    (filter (lambda (p) (not (pi-config-provider-enabled? p)))
                      (pi-config-state-providers st)
                    ))) ;filter
                   ;map
      ) ;
  ;; 夹具里启用 3 个 provider，逐个禁用前两个
  (check (length disabled) => 3)
  (let* ((st1 (pi-config-state (path->string dir) :env '()))
         (plan (pi-config-plan-toggle (pi-config-state (path->string dir) :env '())
                 (pi-config-cursor 1 0) 0
               )) ;pi-config-plan-toggle
        ) ;
    (check (ref plan "ok") => #t)
    ;; 应用后 google/custom-kimi 仍启用，kimi 已关闭；再关剩下两个到最后一个时必须拒绝
    (pi-config-apply-writes st1 (ref plan "writes"))
    (let* ((st2 (pi-config-state (path->string dir) :env '()))
           (plan2 (pi-config-plan-toggle st2 (pi-config-cursor 0 0) 0))
          ) ;
      (check (ref plan2 "ok") => #t)
      (pi-config-apply-writes st2 (ref plan2 "writes"))
      (let* ((st3 (pi-config-state (path->string dir) :env '()))
             (plan3 (pi-config-plan-toggle st3 (pi-config-cursor 0 0) 0))
            ) ;
        (check (ref plan3 "ok") => #f)
        (check (string-contains (ref plan3 "message") "at least one provider") => #t)
      ) ;let*
    ) ;let*
  ) ;let*
) ;let*

;; 7. 启用被禁用的 provider：写入白名单，并创建缺失的 provider 文件
(let* ((dir (make-dir "toggle-provider-on"))
       (st (pi-config-state (path->string dir) :env '()))
       ;; fixture 白名单为 google/kimi-coding/custom-kimi，deepseek 是否启用取决于夹具
       (index (list-index (lambda (p) (not (pi-config-provider-enabled? p)))
                (pi-config-state-providers st)
              ) ;list-index
       ) ;
      ) ;
  (check (number? index) => #t)
  (let* ((provider (list-ref (pi-config-state-providers st) index))
         (p-id (pi-config-provider-id provider))
         (plan (pi-config-plan-toggle st (pi-config-cursor index 0) 0))
         (writes (ref plan "writes"))
         (rels (map car writes))
        ) ;
    (check (ref plan "ok") => #t)
    (check (and (member "models/providers.json" rels) #t) => #t)
    (check (and (member (string-append "models/" p-id ".json") rels) #t) => #t)
    (check (ref plan "message") => (string-append "enabled " p-id " · created models/" p-id ".json"))
  ) ;let*
) ;let*

;; 8. 模型启停：已有白名单时增删
(let* ((dir (make-dir "toggle-model"))
       (st (pi-config-state (path->string dir) :env '()))
       ;; kimi-coding 白名单只有 k3.1-pro，其余内置模型为 ×
       (models (pi-config-provider-models (pi-config-provider st "kimi-coding")))
       (disabled-index (list-index (lambda (m) (not (pi-config-model-enabled? m))) models))
       (plan (pi-config-plan-toggle st (pi-config-cursor (p-index st "kimi-coding") disabled-index) 1))
       (text (wtext plan "models/kimi-coding.json"))
      ) ;
  (check (ref plan "ok") => #t)
  (check (string-contains text "\"k3.1-pro\"") => #t)
  ;; 被启用的模型 id 也写进去了
  (check (string-contains text (pi-config-model-id (list-ref models disabled-index))) => #t)
) ;let*

;; 9. 模型禁用：从白名单移除；移除唯一模型后白名单为空数组
(let* ((dir (make-dir "toggle-model-off"))
       (st (pi-config-state (path->string dir) :env '()))
       (plan (pi-config-plan-toggle st (pi-config-cursor (p-index st "kimi-coding") 0) 1))
       (text (wtext plan "models/kimi-coding.json"))
      ) ;
  (check (ref plan "ok") => #t)
  ;; 白名单里原本只有 k3.1-pro（外加夹具里的无效 id），禁用后 k3.1-pro 被移除
  (check (string-contains text "\"k3.1-pro\"") => #f)
  (check (string-contains text "\"models\": [") => #t)
) ;let*

;; 10. provider 被禁用时模型行的空格：拒绝
(let* ((dir (make-dir "toggle-model-disabled-provider"))
       (st (pi-config-state (path->string dir) :env '()))
       (index (list-index (lambda (p) (not (pi-config-provider-enabled? p)))
                (pi-config-state-providers st)
              ) ;list-index
       )) ;
  (let ((plan (pi-config-plan-toggle st (pi-config-cursor index 0) 1)))
    (check (ref plan "ok") => #f)
    (check (string-contains (ref plan "message") "enable it first") => #t)
  ) ;let
) ;let*

;; 11. 无 models 键的 provider：空格 = 新建白名单只含该模型
(let* ((dir (make-dir "toggle-no-whitelist"))
       ;; google 无 models/google.json，但把它启用并加一个只含空内容的 provider 文件
       (rel "models/google.json")
       (abs (path->string (path-join dir (path "models") (path "google.json"))))
       (_ (path-write-text (path abs)
            (string-append (json->string (list (cons "$schema" "https://case-lambda.org/agent/provider.schema.json")))
              "\n"
            ) ;string-append
          ) ;path-write-text
       ) ;
       (st (pi-config-state (path->string dir) :env '()))
       (plan (pi-config-plan-toggle st (pi-config-cursor (p-index st "google") 0) 1))
       (text (wtext plan rel))
      ) ;
  (check (ref plan "ok") => #t)
  (check (string-contains (ref plan "message") "whitelist created") => #t)
  ;; 只含当前这一个模型
  (let* ((model-id (pi-config-model-id (car (pi-config-provider-models
                                               (pi-config-provider st "google"))))
                                              ;pi-config-provider-models
                                         ;car
                     ;pi-config-model-id
         ) ;
        ) ;
    (check (string-contains text (string-append "\"" model-id "\"")) => #t)
  ) ;let*
) ;let*

;; ---------- 落盘 ----------

;; 12. apply-writes 后文件内容与计划一致
(let* ((dir (make-dir "apply"))
       (st (pi-config-state (path->string dir) :env '()))
       (plan (pi-config-plan-toggle st (pi-config-cursor (p-index st "kimi-coding") 0) 0))
       (res (pi-config-apply-writes st (ref plan "writes")))
       (abs (path->string (path-join dir (path "models") (path "providers.json"))))
       (text (path-read-text (path abs)))
      ) ;
  (check (ref res "ok") => #t)
  (check (string-contains text "kimi-coding") => #f)
  (check (string-contains text "google") => #t)
  ;; 落盘后可重新解析为 JSON
  (check (vector? (string->json text)) => #t)
) ;let*

;; 13. apply-writes 会创建缺失的子目录（模型文件写入路径）
(let* ((dir (make-dir "apply-mkdir"))
       (st (pi-config-state (path->string dir) :env '()))
       (writes (list (cons "models/newp/newm.json" "{\"id\": \"newm\"}\n")))
       (res (pi-config-apply-writes st writes))
       (abs (path->string (path-join dir (path "models") (path "newp") (path "newm.json"))))
      ) ;
  (check (ref res "ok") => #t)
  (check (path-exists? (path abs)) => #t)
) ;let*

;; 15. 设默认模型：写 settings.json，保留其余键
(let* ((dir (make-dir "set-default"))
       (st (pi-config-state (path->string dir) :env '()))
       ;; google 的第一个模型（默认 provider 是 kimi-coding）
       (plan (pi-config-plan-set-default st (pi-config-cursor (p-index st "google") 0) 1))
       (text (wtext plan "settings.json"))
      ) ;
  (check (ref plan "ok") => #t)
  (check (string-contains text "\"defaultProvider\": \"google\"") => #t)
  (check (string-contains text "\"defaultModel\": \"deep-research-max-preview-04-2026\"") => #t)
  ;; 其余键保留
  (check (string-contains text "\"httpProxy\"") => #t)
) ;let*

;; 16. 设默认：被禁用的模型拒绝
(let* ((dir (make-dir "set-default-disabled"))
       (st (pi-config-state (path->string dir) :env '()))
       (models (pi-config-provider-models (pi-config-provider st "kimi-coding")))
       (index (list-index (lambda (m) (not (pi-config-model-enabled? m))) models))
       (plan (pi-config-plan-set-default st (pi-config-cursor (p-index st "kimi-coding") index) 1))
      ) ;
  (check (ref plan "ok") => #f)
  (check (string-contains (ref plan "message") "not enabled") => #t)
) ;let*

;; 17. 设默认：provider 列拒绝（Enter 在 provider 列是"进入"）
(let* ((dir (make-dir "set-default-provider-col"))
       (st (pi-config-state (path->string dir) :env '()))
       (plan (pi-config-plan-set-default st (pi-config-cursor (p-index st "kimi-coding") 0) 0))
      ) ;
  (check (ref plan "ok") => #f)
) ;let*

;; ---------- JSON 序列化 ----------

;; 14. 2 空格缩进、保序、数组内联
(check (pi-config-json-pretty (vector "a" "b")) => "[\"a\", \"b\"]")
(check (pi-config-json-pretty (list (cons "k" (vector "a"))))
  => "{\n  \"k\": [\"a\"]\n}"
) ;check
(check (pi-config-json-pretty (list (cons "n" #t) (cons "m" 1)))
  => "{\n  \"n\": true,\n  \"m\": 1\n}"
) ;check

(check-report)
