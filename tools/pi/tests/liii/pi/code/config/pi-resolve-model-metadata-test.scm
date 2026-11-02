;; pi-resolve-model-metadata
;; 解析模型的 base URL、上下文窗口和最大输出 token 等元数据。
;;
;; 语法
;; ----
;; (pi-resolve-model-metadata provider model-id models-store . opt-env)
;;
;; 参数
;; ----
;; provider : string | symbol
;; 模型供应商名称。
;; model-id : string
;; 模型标识。
;; models-store : alist
;; provider 到模型条目的模型目录。
;; opt-env : alist
;; 可选环境变量表，用于覆盖默认配置。
;;
;; 返回值
;; ----
;; pi-model-metadata
;; 包含模型名称、provider、base-url、context-window 和 max-tokens 的 record。

(import (scheme process-context)
        (liii check)
        (liii os)
        (liii pi ai config record)
        (liii pi code config))

(check-set-mode! 'report-failed)

;; meta 为 record，先转成 alist 再按符号键读取
(define (meta-ref meta key)
  (cdr (assoc key (pi-model-metadata->alist meta))
  ) ;cdr
) ;define

; 模型目录样本（不再读取 models-store.json / models.json）
(define sample-catalog
  (list
    (cons "google"
      (list (cons "models"
              (list (list (cons "id" "gemini-3.8-flash")
                          (cons "name" "Gemini 3.8 Flash")
                          (cons "baseUrl" "https://generativelanguage.googleapis.com/v1beta")
                          (cons "contextWindow" 1048576)
                          (cons "maxTokens" 65536))
                    (list (cons "id" "gemini-2.5-pro")
                          (cons "name" "Gemini 2.5 Pro")
                          (cons "contextWindow" 2097152)
                          (cons "maxTokens" 65536))))))
    (cons "deepseek"
      (list (cons "models"
              (list (list (cons "id" "deepseek-v4-pro")
                          (cons "name" "DeepSeek V4 Pro (fixture)")
                          (cons "baseUrl" "https://api.deepseek.com")
                          (cons "contextWindow" 1000000)
                          (cons "maxTokens" 384000))))))
    (cons "kimi-coding"
      (list (cons "models"
              (list (list (cons "id" "kimi-for-coding")
                          (cons "name" "Kimi K2.7 Code (fixture)")
                          (cons "baseUrl" "https://api.kimi.com/coding")
                          (cons "contextWindow" 262144)
                          (cons "maxTokens" 32768))))))
    (cons "zai-coding-cn"
      (list (cons "models"
              (list (list (cons "id" "glm-5.3") (cons "name" "GLM 5.3 (fixture)")
                          (cons "contextWindow" 1000000) (cons "maxTokens" 131072))
                    (list (cons "id" "glm-5.3-flash") (cons "contextWindow" 1000000)
                          (cons "maxTokens" 131072))
                    (list (cons "id" "glm-5-turbo") (cons "contextWindow" 200000)
                          (cons "maxTokens" 131072))
                    (list (cons "id" "glm-4.5-air") (cons "contextWindow" 128000)
                          (cons "maxTokens" 16384))))))))

;; 哨兵 env 表：非空但不含任何目标键。
;; lookup-env-var 以 (pair? custom-env) 判定是否回落真实进程环境：
;; 省略 env 实参时 custom-env 为空列表会读真实环境，显式传哨兵表确保只查哨兵表。
(define NO-ENV '(("__PI_TEST_NO_ENV__" . "1")))

;; 真实进程环境污染项
(define POLLUTION
  '(("ANTHROPIC_BASE_URL" . "https://polluted.example.com/anthropic")
    ("DEEPSEEK_BASE_URL" . "https://polluted.example.com/deepseek")
    ("GF_PI_BASE_URL" . "https://polluted.example.com/gf-pi")
    ("GF_PI_CONTEXT_WINDOW" . "1")
    ("GF_PI_MAX_TOKENS" . "1")))

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

(let ((models-store sample-catalog))
  ;; 精确匹配已缓存模型
  (let ((meta (pi-resolve-model-metadata "google" "gemini-3.8-flash" models-store (list NO-ENV))))
    (check-true (pi-model-metadata? meta))
    (check (pi-model-metadata-id meta) => "gemini-3.8-flash")
    (check (pi-model-metadata-name meta) => "Gemini 3.8 Flash")
    (check (pi-model-metadata-context-window meta) => 1048576)
    (check (pi-model-metadata-max-tokens meta) => 65536)
    (check (meta-ref meta 'matched?) => #t)
    (check (meta-ref meta 'id) => "gemini-3.8-flash")
    (check (meta-ref meta 'name) => "Gemini 3.8 Flash")
    (check (meta-ref meta 'base-url) => "https://generativelanguage.googleapis.com/v1beta")
    (check (meta-ref meta 'context-window) => 1048576)
    (check (meta-ref meta 'max-tokens) => 65536))

  ;; 未匹配模型走兜底值
  (let ((meta (pi-resolve-model-metadata "google" "custom-model-id" models-store (list NO-ENV))))
    (check (meta-ref meta 'matched?) => #f)
    (check (meta-ref meta 'id) => "custom-model-id")
    (check (meta-ref meta 'name) => "custom-model-id")
    (check (meta-ref meta 'base-url) => "https://generativelanguage.googleapis.com/v1beta")
    (check (meta-ref meta 'context-window) => 128000)
    (check (meta-ref meta 'max-tokens) => 16384))

  ;; 环境变量覆盖
  (let* ((override-env '(("GF_PI_BASE_URL" . "http://localhost:8080/v1beta")
                         ("GF_PI_CONTEXT_WINDOW" . "500000")
                         ("GF_PI_MAX_TOKENS" . "32000")))
         (meta (pi-resolve-model-metadata "google" "gemini-3.8-flash" models-store override-env)))
    (check (meta-ref meta 'base-url) => "http://localhost:8080/v1beta")
    (check (meta-ref meta 'context-window) => 500000)
    (check (meta-ref meta 'max-tokens) => 32000))

  ;; kimi-coding 默认配置兜底
  (let ((meta (pi-resolve-model-metadata "kimi-coding" "kimi-for-coding" '() (list NO-ENV))))
    (check (meta-ref meta 'matched?) => #f)
    (check (meta-ref meta 'id) => "kimi-for-coding")
    (check (meta-ref meta 'base-url) => "https://api.kimi.com/coding/v1")
    (check (meta-ref meta 'context-window) => 262144)
    (check (meta-ref meta 'max-tokens) => 8192))

  ;; zai-coding-cn 默认配置兜底
  (let ((meta (pi-resolve-model-metadata "zai-coding-cn" "glm-4-plus" '() (list NO-ENV))))
    (check (meta-ref meta 'matched?) => #f)
    (check (meta-ref meta 'id) => "glm-4-plus")
    (check (meta-ref meta 'base-url) => "https://open.bigmodel.cn/api/coding/paas/v4")
    (check (meta-ref meta 'context-window) => 1000000)
    (check (meta-ref meta 'max-tokens) => 131072))

  ;; glm 通过 ANTHROPIC_BASE_URL 覆盖端点
  (let* ((env '(("ANTHROPIC_BASE_URL" . "https://custom.bigmodel.cn/api/anthropic")))
         (meta (pi-resolve-model-metadata "zai-coding-cn" "glm-4-plus" '() env)))
    (check (meta-ref meta 'base-url) => "https://custom.bigmodel.cn/api/anthropic"))

  ;; glm 通过 GF_PI_BASE_URL 覆盖端点
  (let* ((env '(("GF_PI_BASE_URL" . "https://proxy.example.com/api/anthropic")))
         (meta (pi-resolve-model-metadata "zai-coding-cn" "glm-4-plus" '() env)))
    (check (meta-ref meta 'base-url) => "https://proxy.example.com/api/anthropic"))

  ;; deepseek 默认配置兜底
  (let ((meta (pi-resolve-model-metadata "deepseek" "deepseek-chat" '() (list NO-ENV))))
    (check (meta-ref meta 'matched?) => #f)
    (check (meta-ref meta 'id) => "deepseek-chat")
    (check (meta-ref meta 'base-url) => "https://api.deepseek.com/anthropic")
    (check (meta-ref meta 'context-window) => 128000)
    (check (meta-ref meta 'max-tokens) => 8192))

  ;; deepseek 通过 DEEPSEEK_BASE_URL 覆盖端点
  (let* ((env '(("DEEPSEEK_BASE_URL" . "https://custom.deepseek.com/v1")))
         (meta (pi-resolve-model-metadata "deepseek" "deepseek-chat" '() env)))
    (check (meta-ref meta 'base-url) => "https://custom.deepseek.com/v1"))

  ;; deepseek 通过 ANTHROPIC_BASE_URL 覆盖端点
  (let* ((env '(("ANTHROPIC_BASE_URL" . "https://custom.deepseek.com/anthropic")))
         (meta (pi-resolve-model-metadata "deepseek" "deepseek-chat" '() env)))
    (check (meta-ref meta 'base-url) => "https://custom.deepseek.com/anthropic"))

  ;; deepseek 通过 GF_PI_BASE_URL 覆盖端点
  (let* ((env '(("GF_PI_BASE_URL" . "https://proxy.example.com/anthropic")))
         (meta (pi-resolve-model-metadata "deepseek" "deepseek-chat" '() env)))
    (check (meta-ref meta 'base-url) => "https://proxy.example.com/anthropic")))

;; 自定义目录中的 glm-5.3 系列命中正确元数据
(let ((merged sample-catalog))
  (let ((meta (pi-resolve-model-metadata "zai-coding-cn" "glm-5.3" merged (list NO-ENV))))
    (check (meta-ref meta 'matched?) => #t)
    (check (meta-ref meta 'id) => "glm-5.3")
    (check (meta-ref meta 'base-url) => "https://open.bigmodel.cn/api/coding/paas/v4")
    (check (meta-ref meta 'context-window) => 1000000)
    (check (meta-ref meta 'max-tokens) => 131072))
  (let ((meta (pi-resolve-model-metadata "zai-coding-cn" "glm-5.3-flash" merged (list NO-ENV))))
    (check (meta-ref meta 'matched?) => #t)
    (check (meta-ref meta 'context-window) => 1000000)
    (check (meta-ref meta 'max-tokens) => 131072))
  (let ((meta (pi-resolve-model-metadata "zai-coding-cn" "glm-5-turbo" merged (list NO-ENV))))
    (check (meta-ref meta 'matched?) => #t)
    (check (meta-ref meta 'context-window) => 200000)
    (check (meta-ref meta 'max-tokens) => 131072))
  (let ((meta (pi-resolve-model-metadata "zai-coding-cn" "glm-4.5-air" merged (list NO-ENV))))
    (check (meta-ref meta 'matched?) => #t)
    (check (meta-ref meta 'context-window) => 128000)
    (check (meta-ref meta 'max-tokens) => 16384)))

;; minimax-cn 内置模型兜底元数据（任务 030）
(let ((meta (pi-resolve-model-metadata "minimax-cn" "MiniMax-M3.1-Flash" '() (list NO-ENV))))
  (check (meta-ref meta 'matched?) => #t)
  (check (meta-ref meta 'id) => "MiniMax-M3.1-Flash")
  (check (meta-ref meta 'base-url) => "https://api.minimaxi.com/anthropic")
  (check (meta-ref meta 'context-window) => 1000000)
  (check (meta-ref meta 'max-tokens) => 524288))

(let ((meta (pi-resolve-model-metadata "minimax-cn" "MiniMax-M3" '() (list NO-ENV))))
  (check (meta-ref meta 'matched?) => #t)
  (check (meta-ref meta 'id) => "MiniMax-M3")
  (check (meta-ref meta 'base-url) => "https://api.minimaxi.com/anthropic")
  (check (meta-ref meta 'context-window) => 1048576)
  (check (meta-ref meta 'max-tokens) => 512000))

(let ((meta (pi-resolve-model-metadata "minimax-cn" "MiniMax-M2.7" '() (list NO-ENV))))
  (check (meta-ref meta 'matched?) => #t)
  (check (meta-ref meta 'base-url) => "https://api.minimaxi.com/anthropic")
  (check (meta-ref meta 'context-window) => 204800)
  (check (meta-ref meta 'max-tokens) => 131072))

(let ((meta (pi-resolve-model-metadata "minimax-cn" "MiniMax-M2.7-highspeed" '() (list NO-ENV))))
  (check (meta-ref meta 'matched?) => #t)
  (check (meta-ref meta 'base-url) => "https://api.minimaxi.com/anthropic")
  (check (meta-ref meta 'context-window) => 204800)
  (check (meta-ref meta 'max-tokens) => 131072))

;; kimi 旧 baseUrl 带尾斜杠时也能归一（URI 层面比较）
(let* ((k-store
         '(("kimi-coding" . (("models" . #((("id" . "kimi-for-coding")
                                          ("baseUrl" . "https://api.kimi.com/coding/")
                                          ("contextWindow" . 262144)
                                          ("maxTokens" . 8192))))))))
       (meta (pi-resolve-model-metadata "kimi-coding" "kimi-for-coding" k-store (list NO-ENV))))
  (check (meta-ref meta 'base-url) => "https://api.kimi.com/coding/v1"))

;; deepseek 旧 baseUrl 显式默认端口时也能归一
(let* ((ds-store
         '(("deepseek" . (("models" . #((("id" . "deepseek-chat")
                                       ("baseUrl" . "https://api.deepseek.com:443")
                                       ("contextWindow" . 128000)
                                       ("maxTokens" . 8192))))))))
       (meta (pi-resolve-model-metadata "deepseek" "deepseek-chat" ds-store (list NO-ENV))))
  (check (meta-ref meta 'base-url) => "https://api.deepseek.com/anthropic"))

;; minimax-cn 端点环境变量覆盖（MINIMAX_BASE_URL 作为回退变量仍生效）
(let* ((env '(("MINIMAX_BASE_URL" . "https://proxy.example.com/minimax")))
       (meta (pi-resolve-model-metadata "minimax-cn" "MiniMax-M3.1-Flash" '() env)))
  (check (meta-ref meta 'base-url) => "https://proxy.example.com/minimax"))

(let* ((env '(("MINIMAX_CN_BASE_URL" . "https://proxy.example.com/minimax-cn")))
       (meta (pi-resolve-model-metadata "minimax-cn" "MiniMax-M3.1-Flash" '() env)))
  (check (meta-ref meta 'base-url) => "https://proxy.example.com/minimax-cn"))

(let* ((env '(("GF_PI_BASE_URL" . "https://proxy.example.com/mm")))
       (meta (pi-resolve-model-metadata "minimax-cn" "MiniMax-M3.1-Flash" '() env)))
  (check (meta-ref meta 'base-url) => "https://proxy.example.com/mm"))

;; 污染环境下解析结果不变（防回归：真实进程环境不得影响模型元数据解析）
(let* ((merged sample-catalog)
       (meta (with-env-vars POLLUTION
                (lambda ()
                  (pi-resolve-model-metadata "deepseek" "deepseek-v4-pro" merged (list NO-ENV))
                ) ;lambda
              ) ;with-env-vars
              )) ;
  (check (meta-ref meta 'matched?) => #t)
  (check (meta-ref meta 'base-url) => "https://api.deepseek.com/anthropic")
  (check (meta-ref meta 'context-window) => 1000000)
  (check (meta-ref meta 'max-tokens) => 384000))

;; 污染环境下 zai-coding-cn 兜底端点不被真实环境的 ANTHROPIC_BASE_URL 覆盖
(let ((meta (with-env-vars POLLUTION
              (lambda ()
                (pi-resolve-model-metadata "zai-coding-cn" "glm-4-plus" '() (list NO-ENV))
              ) ;lambda
            ) ;with-env-vars
            )) ;
  (check (meta-ref meta 'base-url) => "https://open.bigmodel.cn/api/coding/paas/v4")
  (check (meta-ref meta 'context-window) => 1000000)
  (check (meta-ref meta 'max-tokens) => 131072))

(check-report)
