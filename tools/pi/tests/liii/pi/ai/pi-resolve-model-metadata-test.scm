(import (scheme process-context)
        (liii check)
        (liii os)
        (liii pi ai config record) (liii pi ai event) (liii pi ai models) (liii pi ai template))

(check-set-mode! 'report-failed)

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
;; 已缓存的 provider 和模型元数据目录。
;; opt-env : alist
;; 可选环境变量表，用于覆盖默认配置。
;;
;; 返回值
;; ----
;; pi-model-metadata
;; 包含模型名称、provider、base-url、context-window 和 max-tokens 的 record。
;;
;; 说明
;; ----
;; 优先使用 models-store 中的精确模型匹配，缺失时使用 provider 默认值。
;; 支持旧 base URL 归一化及 GF_PI_* 和 provider 特定环境变量覆盖。
;;
;; 示例
;; ----
;; (pi-model-metadata-id (pi-resolve-model-metadata "google" "gemini" store)) => "gemini"
(define fixture-dir
  (if (file-exists? "tests/fixtures/default")
    "tests/fixtures/default"
    "tools/pi/tests/fixtures/default"
  ) ;if
) ;define

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

(let ((models-store (pi-load-models-store fixture-dir)))
  ;; 精确匹配已缓存模型
  (let ((meta (pi-resolve-model-metadata "google" "gemini-3.8-flash" models-store (list NO-ENV))))
    (check-true (pi-model-metadata? meta))
    (check (pi-model-metadata-id meta) => "gemini-3.8-flash")
    (check (pi-model-metadata-name meta) => "Gemini 3.8 Flash")
    (check (pi-model-metadata-context-window meta) => 1048576)
    (check (pi-model-metadata-max-tokens meta) => 65536)
    (check (cdr (assoc 'matched? meta)) => #t)
    (check (cdr (assoc 'id meta)) => "gemini-3.8-flash")
    (check (cdr (assoc 'name meta)) => "Gemini 3.8 Flash")
    (check (cdr (assoc 'base-url meta)) => "https://generativelanguage.googleapis.com/v1beta")
    (check (cdr (assoc 'context-window meta)) => 1048576)
    (check (cdr (assoc 'max-tokens meta)) => 65536))

  ;; 未匹配模型走兜底值
  (let ((meta (pi-resolve-model-metadata "google" "custom-model-id" models-store (list NO-ENV))))
    (check (cdr (assoc 'matched? meta)) => #f)
    (check (cdr (assoc 'id meta)) => "custom-model-id")
    (check (cdr (assoc 'name meta)) => "custom-model-id")
    (check (cdr (assoc 'base-url meta)) => "https://generativelanguage.googleapis.com/v1beta")
    (check (cdr (assoc 'context-window meta)) => 128000)
    (check (cdr (assoc 'max-tokens meta)) => 16384))

  ;; 环境变量覆盖
  (let* ((override-env '(("GF_PI_BASE_URL" . "http://localhost:8080/v1beta")
                         ("GF_PI_CONTEXT_WINDOW" . "500000")
                         ("GF_PI_MAX_TOKENS" . "32000")))
         (meta (pi-resolve-model-metadata "google" "gemini-3.8-flash" models-store override-env)))
    (check (cdr (assoc 'base-url meta)) => "http://localhost:8080/v1beta")
    (check (cdr (assoc 'context-window meta)) => 500000)
    (check (cdr (assoc 'max-tokens meta)) => 32000))

  ;; kimi-coding 默认配置兜底
  (let ((meta (pi-resolve-model-metadata "kimi-coding" "kimi-for-coding" '() (list NO-ENV))))
    (check (cdr (assoc 'matched? meta)) => #f)
    (check (cdr (assoc 'id meta)) => "kimi-for-coding")
    (check (cdr (assoc 'base-url meta)) => "https://api.kimi.com/coding/v1")
    (check (cdr (assoc 'context-window meta)) => 262144)
    (check (cdr (assoc 'max-tokens meta)) => 8192))

  ;; glm 默认配置兜底
  (let ((meta (pi-resolve-model-metadata "glm" "glm-4-plus" '() (list NO-ENV))))
    (check (cdr (assoc 'matched? meta)) => #f)
    (check (cdr (assoc 'id meta)) => "glm-4-plus")
    (check (cdr (assoc 'base-url meta)) => "https://open.bigmodel.cn/api/anthropic")
    (check (cdr (assoc 'context-window meta)) => 128000)
    (check (cdr (assoc 'max-tokens meta)) => 8192))

  ;; glm 通过 ANTHROPIC_BASE_URL 覆盖端点
  (let* ((env '(("ANTHROPIC_BASE_URL" . "https://custom.bigmodel.cn/api/anthropic")))
         (meta (pi-resolve-model-metadata "glm" "glm-4-plus" '() env)))
    (check (cdr (assoc 'base-url meta)) => "https://custom.bigmodel.cn/api/anthropic"))

  ;; glm 通过 GF_PI_BASE_URL 覆盖端点
  (let* ((env '(("GF_PI_BASE_URL" . "https://proxy.example.com/api/anthropic")))
         (meta (pi-resolve-model-metadata "glm" "glm-4-plus" '() env)))
    (check (cdr (assoc 'base-url meta)) => "https://proxy.example.com/api/anthropic"))

  ;; deepseek 默认配置兜底
  (let ((meta (pi-resolve-model-metadata "deepseek" "deepseek-chat" '() (list NO-ENV))))
    (check (cdr (assoc 'matched? meta)) => #f)
    (check (cdr (assoc 'id meta)) => "deepseek-chat")
    (check (cdr (assoc 'base-url meta)) => "https://api.deepseek.com/anthropic")
    (check (cdr (assoc 'context-window meta)) => 128000)
    (check (cdr (assoc 'max-tokens meta)) => 8192))

  ;; deepseek 通过 DEEPSEEK_BASE_URL 覆盖端点
  (let* ((env '(("DEEPSEEK_BASE_URL" . "https://custom.deepseek.com/v1")))
         (meta (pi-resolve-model-metadata "deepseek" "deepseek-chat" '() env)))
    (check (cdr (assoc 'base-url meta)) => "https://custom.deepseek.com/v1"))

  ;; deepseek 通过 ANTHROPIC_BASE_URL 覆盖端点
  (let* ((env '(("ANTHROPIC_BASE_URL" . "https://custom.deepseek.com/anthropic")))
         (meta (pi-resolve-model-metadata "deepseek" "deepseek-chat" '() env)))
    (check (cdr (assoc 'base-url meta)) => "https://custom.deepseek.com/anthropic"))

  ;; deepseek 通过 GF_PI_BASE_URL 覆盖端点
  (let* ((env '(("GF_PI_BASE_URL" . "https://proxy.example.com/anthropic")))
         (meta (pi-resolve-model-metadata "deepseek" "deepseek-chat" '() env)))
    (check (cdr (assoc 'base-url meta)) => "https://proxy.example.com/anthropic")))

;; models.json 合并目录后 glm-5.3 系列命中正确元数据（任务 028）
(let ((merged (pi-merge-models-json-into-store (pi-load-models-store fixture-dir)
                (pi-load-models-json fixture-dir))))
  (let ((meta (pi-resolve-model-metadata "glm" "glm-5.3" merged (list NO-ENV))))
    (check (cdr (assoc 'matched? meta)) => #t)
    (check (cdr (assoc 'id meta)) => "glm-5.3")
    (check (cdr (assoc 'base-url meta)) => "https://open.bigmodel.cn/api/anthropic")
    (check (cdr (assoc 'context-window meta)) => 1000000)
    (check (cdr (assoc 'max-tokens meta)) => 131072))
  (let ((meta (pi-resolve-model-metadata "glm" "glm-5.3-flash" merged (list NO-ENV))))
    (check (cdr (assoc 'matched? meta)) => #t)
    (check (cdr (assoc 'context-window meta)) => 1000000)
    (check (cdr (assoc 'max-tokens meta)) => 131072))
  (let ((meta (pi-resolve-model-metadata "glm" "glm-5-turbo" merged (list NO-ENV))))
    (check (cdr (assoc 'matched? meta)) => #t)
    (check (cdr (assoc 'context-window meta)) => 200000)
    (check (cdr (assoc 'max-tokens meta)) => 131072))
  (let ((meta (pi-resolve-model-metadata "glm" "glm-4.5-air" merged (list NO-ENV))))
    (check (cdr (assoc 'matched? meta)) => #t)
    (check (cdr (assoc 'context-window meta)) => 128000)
    (check (cdr (assoc 'max-tokens meta)) => 16384)))

;; minimax 内置模型兜底元数据（任务 030）
(let ((meta (pi-resolve-model-metadata "minimax" "MiniMax-M3.1-Flash" '() (list NO-ENV))))
  (check (cdr (assoc 'matched? meta)) => #t)
  (check (cdr (assoc 'id meta)) => "MiniMax-M3.1-Flash")
  (check (cdr (assoc 'base-url meta)) => "https://api.minimaxi.com/anthropic")
  (check (cdr (assoc 'context-window meta)) => 1000000)
  (check (cdr (assoc 'max-tokens meta)) => 524288))

(let ((meta (pi-resolve-model-metadata "minimax" "MiniMax-M3" '() (list NO-ENV))))
  (check (cdr (assoc 'matched? meta)) => #t)
  (check (cdr (assoc 'id meta)) => "MiniMax-M3")
  (check (cdr (assoc 'base-url meta)) => "https://api.minimaxi.com/anthropic")
  (check (cdr (assoc 'context-window meta)) => 1048576)
  (check (cdr (assoc 'max-tokens meta)) => 512000))

(let ((meta (pi-resolve-model-metadata "minimax" "MiniMax-M2.7" '() (list NO-ENV))))
  (check (cdr (assoc 'matched? meta)) => #t)
  (check (cdr (assoc 'base-url meta)) => "https://api.minimaxi.com/anthropic")
  (check (cdr (assoc 'context-window meta)) => 204800)
  (check (cdr (assoc 'max-tokens meta)) => 131072))

(let ((meta (pi-resolve-model-metadata "minimax-cn" "MiniMax-M2.7-highspeed" '() (list NO-ENV))))
  (check (cdr (assoc 'matched? meta)) => #t)
  (check (cdr (assoc 'base-url meta)) => "https://api.minimaxi.com/anthropic")
  (check (cdr (assoc 'context-window meta)) => 204800)
  (check (cdr (assoc 'max-tokens meta)) => 131072))

;; minimax models-store 中的国际站 baseUrl 归一为大陆站
(let* ((mm-store
         '(("minimax" . (("models" . #((("id" . "MiniMax-M3")
                                         ("baseUrl" . "https://api.minimax.io/anthropic")
                                         ("contextWindow" . 1000000)
                                         ("maxTokens" . 512000))))))))
       (meta (pi-resolve-model-metadata "minimax" "MiniMax-M3" mm-store (list NO-ENV))))
  (check (cdr (assoc 'matched? meta)) => #t)
  (check (cdr (assoc 'base-url meta)) => "https://api.minimaxi.com/anthropic")
  (check (cdr (assoc 'context-window meta)) => 1000000)
  (check (cdr (assoc 'max-tokens meta)) => 512000))

;; 旧 baseUrl 带尾斜杠时也能归一（URI 层面比较）
(let* ((mm-store
         '(("minimax" . (("models" . #((("id" . "MiniMax-M3")
                                        ("baseUrl" . "https://api.minimax.io/")
                                        ("contextWindow" . 1000000)
                                        ("maxTokens" . 512000))))))))
       (meta (pi-resolve-model-metadata "minimax" "MiniMax-M3" mm-store (list NO-ENV))))
  (check (cdr (assoc 'base-url meta)) => "https://api.minimaxi.com/anthropic"))

;; kimi 旧 baseUrl 带尾斜杠时也能归一
(let* ((k-store
         '(("kimi-coding" . (("models" . #((("id" . "kimi-for-coding")
                                          ("baseUrl" . "https://api.kimi.com/coding/")
                                          ("contextWindow" . 262144)
                                          ("maxTokens" . 8192))))))))
       (meta (pi-resolve-model-metadata "kimi-coding" "kimi-for-coding" k-store (list NO-ENV))))
  (check (cdr (assoc 'base-url meta)) => "https://api.kimi.com/coding/v1"))

;; deepseek 旧 baseUrl 显式默认端口时也能归一
(let* ((ds-store
         '(("deepseek" . (("models" . #((("id" . "deepseek-chat")
                                       ("baseUrl" . "https://api.deepseek.com:443")
                                       ("contextWindow" . 128000)
                                       ("maxTokens" . 8192))))))))
       (meta (pi-resolve-model-metadata "deepseek" "deepseek-chat" ds-store (list NO-ENV))))
  (check (cdr (assoc 'base-url meta)) => "https://api.deepseek.com/anthropic"))

;; minimax 端点环境变量覆盖
(let* ((env '(("MINIMAX_BASE_URL" . "https://proxy.example.com/minimax")))
       (meta (pi-resolve-model-metadata "minimax" "MiniMax-M3.1-Flash" '() env)))
  (check (cdr (assoc 'base-url meta)) => "https://proxy.example.com/minimax"))

(let* ((env '(("MINIMAX_CN_BASE_URL" . "https://proxy.example.com/minimax-cn")))
       (meta (pi-resolve-model-metadata "minimax-cn" "MiniMax-M3.1-Flash" '() env)))
  (check (cdr (assoc 'base-url meta)) => "https://proxy.example.com/minimax-cn"))

(let* ((env '(("GF_PI_BASE_URL" . "https://proxy.example.com/mm")))
       (meta (pi-resolve-model-metadata "minimax" "MiniMax-M3.1-Flash" '() env)))
  (check (cdr (assoc 'base-url meta)) => "https://proxy.example.com/mm"))

;; 污染环境下解析结果不变（防回归：真实进程环境不得影响模型元数据解析）
(let* ((merged (pi-merge-models-json-into-store (pi-load-models-store fixture-dir)
                (pi-load-models-json fixture-dir)))
       (meta (with-env-vars POLLUTION
                (lambda ()
                  (pi-resolve-model-metadata "deepseek" "deepseek-v4-pro" merged (list NO-ENV))
                ) ;lambda
              ) ;with-env-vars
              )) ;
  (check (cdr (assoc 'matched? meta)) => #t)
  (check (cdr (assoc 'base-url meta)) => "https://api.deepseek.com/anthropic")
  (check (cdr (assoc 'context-window meta)) => 1000000)
  (check (cdr (assoc 'max-tokens meta)) => 131072))

;; 污染环境下 glm 兜底端点不被真实环境的 ANTHROPIC_BASE_URL 覆盖
(let ((meta (with-env-vars POLLUTION
              (lambda ()
                (pi-resolve-model-metadata "glm" "glm-4-plus" '() (list NO-ENV))
              ) ;lambda
            ) ;with-env-vars
            )) ;
  (check (cdr (assoc 'base-url meta)) => "https://open.bigmodel.cn/api/anthropic")
  (check (cdr (assoc 'context-window meta)) => 128000)
  (check (cdr (assoc 'max-tokens meta)) => 8192))

(check-report)
