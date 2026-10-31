(import (scheme file)
        (scheme process-context)
        (liii check)
        (liii os)
        (liii pi ai models catalog))

(check-set-mode! 'report-failed)

;; pi-build-models-catalog
;; 合并内置目录、models-store.json 和 models.json，构造模型目录。
;;
;; 语法
;; ----
;; (pi-build-models-catalog agent-dir)
;;
;; 参数
;; ----
;; agent-dir : string
;; 包含 models-store.json 和可选 models.json 的配置目录。
;;
;; 返回值
;; ----
;; alist
;; provider 到模型列表的目录，provider 名称为排序后的字符串。
;;
;; 说明
;; ----
;; 同一模型的合并优先级为 models.json > models-store.json > 内置目录。
;; provider 名称去重，配置文件不存在时使用可用的底层目录。
;;
;; 示例
;; ----
;; (pi-catalog-provider-names (pi-build-models-catalog "~/.pi/agent")) => '("anthropic" "google" ...)

(define fixture-dir
  (if (file-exists? "tests/fixtures/default")
    "tests/fixtures/default"
    "tools/pi/tests/fixtures/default"))
;; 等价于「无用户模型配置」的自建 fixture（无 models.json / models-store.json）
(define empty-fixture-dir
  (if (file-exists? "tests/fixtures/config-empty")
    "tests/fixtures/config-empty"
    "tools/pi/tests/fixtures/config-empty"))
;; 自建 agent 配置 fixture：模型元数据由仓库内文件钉死
(define config-fixture-dir
  (if (file-exists? "tests/fixtures/config")
    "tests/fixtures/config"
    "tools/pi/tests/fixtures/config"))

(define (with-env-vars vars thunk)
  "把 vars（(名称 . 值) 列表）临时写入真实进程环境，执行 thunk 后完整还原。"
  (let ((saved (map (lambda (kv) (cons (car kv) (get-environment-variable (car kv))))
                    vars)))
    (for-each (lambda (kv) (putenv (car kv) (cdr kv))) vars)
    (let ((result (catch #t thunk (lambda args (apply throw args)))))
      (for-each (lambda (kv)
                  (if (cdr kv)
                    (putenv (car kv) (cdr kv))
                    (unsetenv (car kv))
                  ) ;if
                ) ;for-each
               saved)
      result
    ) ;let
  ) ;let
) ;define

;; 1. 空 agent-dir 时，退化为纯内置目录 (5 个内置 provider)
(let* ((empty-catalog (pi-build-models-catalog empty-fixture-dir))
       (names (pi-catalog-provider-names empty-catalog)))
  (check (length names) => 5)
  (check-true (if (member "google" names) #t #f))
  (check-true (if (member "deepseek" names) #t #f))
  (check-true (if (member "kimi-coding" names) #t #f))
  (check-true (if (member "minimax-cn" names) #t #f))
  (check-true (if (member "minimax-cn" names) #t #f))
  (check-true (if (member "zai-coding-cn" names) #t #f)))

;; 2. 使用 default fixture，验证三层合并与优先级覆盖
(let* ((catalog (pi-build-models-catalog fixture-dir))
       (names (pi-catalog-provider-names catalog)))
  ;; default fixture 的 models.json 含 zai-coding-cn；内置 5 个 + fixture 独有的 provider
  (check (length names) => 6)
  (check-true (if (member "zai-coding-cn" names) #t #f))

  ;; 检查 deepseek 模型的优先级覆盖：models.json 中的 maxTokens 覆盖内置值
  (let* ((ds-models (pi-catalog-provider-models catalog "deepseek"))
         (flash (assoc "deepseek-v4-flash"
                       (map (lambda (m) (cons (cdr (assoc "id" m)) m)) ds-models))))
    (check-true (pair? flash))
    ;; 来自 models.json 的 maxTokens 是 131072，而内置是 384000
    (check (cdr (assoc "maxTokens" (cdr flash))) => 131072)
    ;; models-store 中的 deepseek-flash 也应该被合并进来
    (check-true (if (member "deepseek-flash"
                            (map (lambda (m) (cdr (assoc "id" m))) ds-models))
                    #t
                    #f)))

  ;; 检查 glm 模型 (只在 models.json 中存在，provider 键为 zai-coding-cn)
  (let ((glm-models (pi-catalog-provider-models catalog "zai-coding-cn")))
    (check-true (pair? glm-models))))

;; 3. 自建 fixture 目录覆盖内置模型（断言值由仓库内 fixture 钉死）
(let* ((catalog (pi-build-models-catalog config-fixture-dir))
       (ds-models (pi-catalog-provider-models catalog "deepseek"))
       (pro (assoc "deepseek-v4-pro"
                   (map (lambda (m) (cons (cdr (assoc "id" m)) m)) ds-models))))
  (check-true (pair? pro))
  (check (cdr (assoc "name" (cdr pro))) => "DeepSeek V4 Pro (fixture)")
  (check (cdr (assoc "baseUrl" (cdr pro))) => "https://api.deepseek.com")
  (check (cdr (assoc "contextWindow" (cdr pro))) => 1000000)
  (check (cdr (assoc "maxTokens" (cdr pro))) => 384000))

;; 4. 污染环境下目录构建只认显式 agent-dir（防回归：不得读本机 ~/.pi/agent）
(let* ((catalog (with-env-vars '(("PI_CODING_AGENT_DIR" . "/tmp/pi-polluted-agent-dir"))
                   (lambda () (pi-build-models-catalog config-fixture-dir))
                 ) ;with-env-vars
                ))
  (let* ((ds-models (pi-catalog-provider-models catalog "deepseek"))
         (pro (assoc "deepseek-v4-pro"
                     (map (lambda (m) (cons (cdr (assoc "id" m)) m)) ds-models))))
    (check-true (pair? pro))
    (check (cdr (assoc "name" (cdr pro))) => "DeepSeek V4 Pro (fixture)")))

(check-report)
