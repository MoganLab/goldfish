(import (liii check) (liii pi))

(check-set-mode! 'report-failed)

;; pi-resolve-model-metadata
;; 解析指定模型 ID 的上下文窗口、token 上限与 baseUrl 等元数据。
(define fixture-dir
  (if (file-exists? "tests/fixtures/default")
    "tests/fixtures/default"
    "tools/pi/tests/fixtures/default"
  ) ;if
) ;define

(let ((models-store (pi-load-models-store fixture-dir)))
  ;; 精确匹配已缓存模型
  (let ((meta (pi-resolve-model-metadata "google" "gemini-3.8-flash" models-store '())))
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
  (let ((meta (pi-resolve-model-metadata "google" "custom-model-id" models-store '())))
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
  (let ((meta (pi-resolve-model-metadata "kimi-coding" "kimi-for-coding" '() '())))
    (check (cdr (assoc 'matched? meta)) => #f)
    (check (cdr (assoc 'id meta)) => "kimi-for-coding")
    (check (cdr (assoc 'base-url meta)) => "https://api.kimi.com/coding/v1")
    (check (cdr (assoc 'context-window meta)) => 262144)
    (check (cdr (assoc 'max-tokens meta)) => 8192)))

(check-report)
