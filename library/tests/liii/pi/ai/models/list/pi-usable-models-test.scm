;; pi-usable-models
;; 已激活模型列表（gf pi 的可用 = 白名单激活，不再叠加凭证过滤）：
;; provider 集 = pi-build-user-catalog 的 user catalog（providers.json 供应商白名单，
;; 缺省/空数组即全部内置 provider），模型 = models/<provider>.json 白名单（缺省全量）。
;;
;; 语法
;; ----
;; (pi-usable-models models-store) -> list
;;
;; 参数
;; ----
;; models-store : alist
;; pi-build-user-catalog 构造的用户模型目录。
;;
;; 返回值
;; ----
;; list
;; 条目为 (provider id name reasoning thinking-levels) alist，
;; 按 provider、id 字典序排序。
;;
;; 描述
;; ----
;; 与 gf pi config 三列视图、--list-models 的 ●（已激活）判定一致；
;; 有无凭证不影响列表（缺凭证报错仍在请求期由 config.scm 的 api-key 解析报出）。

(set! *load-path* (append (list "../tools/pi" ".") *load-path*))

(import (scheme base)
  (scheme file)
  (srfi srfi-1)
  (liii check)
  (liii list)
  (liii sort)
  (liii string)
  (liii path)
  (liii shutil)
  (liii pi ai auth)
  (liii pi ai models catalog)
  (liii pi ai models list)
  (liii pi ai models)
  (liii pi ai thinking)
  (liii pi code config user-catalog)
) ;import

(check-set-mode! 'report-failed)

;; 夹具：供应商白名单只激活 google 与 kimi-coding（其余内置 provider 白名单外），
;; google 另有模型白名单只留两个模型；auth.json 只配 google 凭证，
;; 使「已激活但无凭证」的 kimi-coding 与「白名单外」场景同时可断言

(define AGENT-DIR
  (path->string (path-join (path-temp-dir) "pi-usable-models-test"))
) ;define

(when (file-exists? AGENT-DIR)
  (rmtree AGENT-DIR)
) ;when

(path-mkdir AGENT-DIR :parents #t :exist-ok #t)
(path-write-text (path->string (path-join (path AGENT-DIR) (path "auth.json")))
  "{\"google\":{\"type\":\"api_key\",\"key\":\"fixture-google-key\"},\"deepseek\":{\"type\":\"api_key\",\"key\":\"fixture-deepseek-key\"}}"
) ;path-write-text
(path-mkdir (path->string (path-join (path AGENT-DIR) (path "models")))
  :exist-ok #t
) ;path-mkdir
(path-write-text (path->string (path-join (path AGENT-DIR) (path "models") (path "providers.json"))
                 ) ;path->string
  "[\"google\",\"kimi-coding\"]"
) ;path-write-text
(path-write-text (path->string (path-join (path AGENT-DIR) (path "models") (path "google.json")))
  "{\"models\":[\"gemini-2.5-flash\",\"gemini-2.5-pro\"]}"
) ;path-write-text

(let* ((store (pi-build-user-catalog (path->string AGENT-DIR)))
       (models (pi-usable-models store))
       ;; 便于断言的派生视图
       (providers
         (delete-duplicates
           (map (lambda (m) (cdr (assoc 'provider m))) models)
         ) ;delete-duplicates
       ) ;providers
       (keys
         (map
           (lambda (m) (string-append (cdr (assoc 'provider m)) "/" (cdr (assoc 'id m))))
           models
         ) ;map
       ) ;keys
       (gemini-pro
         (find
           (lambda (m) (equal? (cdr (assoc 'id m)) "gemini-2.5-pro"))
           models
         ) ;find
       ) ;gemini-pro
       (kimi-entry
         (find
           (lambda (m) (equal? (cdr (assoc 'provider m)) "kimi-coding"))
           models
         ) ;find
       ) ;kimi-entry
      ) ;

  ;; 1. 已激活模型的 provider 全部返回：无凭证的 kimi-coding 同样返回
  (check-true (pair? models))
  (check providers => '("google" "kimi-coding"))

  ;; 2. 激活门控：白名单外 provider 即使 auth.json 配有凭证也不返回
  (check-false (member "deepseek" providers))

  ;; 3. 模型白名单：google 只剩白名单内两个模型，白名单外模型不返回
  (check (filter (lambda (k) (string-starts? k "google/")) keys)
    =>
    '("google/gemini-2.5-flash" "google/gemini-2.5-pro")
  ) ;check

  ;; 4. 条目字段完整：provider/id/name/reasoning/thinking-levels 齐备
  (check-true (pair? gemini-pro))
  (check (cdr (assoc 'provider gemini-pro)) => "google")
  (check (cdr (assoc 'name gemini-pro)) => "Gemini 2.5 Pro")
  (check (cdr (assoc 'reasoning gemini-pro)) => #t)
  (check (cdr (assoc 'thinking-levels gemini-pro))
    =>
    (pi-model-thinking-levels gemini-pro)
  ) ;check

  ;; 5. 无凭证 provider 的已激活模型条目同样完整
  (check-true (pair? kimi-entry))
  (check (cdr (assoc 'id kimi-entry)) => "k3")

  ;; 6. 排序：provider 字典序，同 provider 内 id 字典序
  (check (length keys) => (length (delete-duplicates keys)))
  (check keys => (list-sort string<? keys))

) ;let*

;; 7. 完全无凭证（auth.json 缺失）时不再是空列表，而是全部已激活模型
(let* ((dir-b (string-append AGENT-DIR "-b"))
       (store-b
         (begin
           (when (file-exists? dir-b)
             (rmtree dir-b)
           ) ;when
           (path-mkdir dir-b :parents #t :exist-ok #t)
           (path-mkdir (path->string (path-join (path dir-b) (path "models")))
             :exist-ok #t
           ) ;path-mkdir
           (path-write-text (path->string (path-join (path dir-b) (path "models") (path "providers.json")))
             "[\"google\"]"
           ) ;path-write-text
           (pi-build-user-catalog dir-b)
         ) ;begin
       ) ;store-b
       (models-b (pi-usable-models store-b))
       (providers-b
         (delete-duplicates
           (map (lambda (m) (cdr (assoc 'provider m))) models-b)
         ) ;delete-duplicates
       ) ;providers-b
      ) ;
  (check-true (pair? models-b))
  (check providers-b => '("google"))
) ;let*

(rmtree AGENT-DIR)
(rmtree (string-append AGENT-DIR "-b"))

(check-report)
