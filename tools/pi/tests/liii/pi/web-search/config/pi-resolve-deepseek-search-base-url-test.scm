(import (liii check)
        (liii pi web-search config)
) ;import

(check-set-mode! 'report-failed)

;; pi-resolve-deepseek-search-base-url
(check (pi-resolve-deepseek-search-base-url '() #f) => "https://api.deepseek.com")
(check (pi-resolve-deepseek-search-base-url '() "http://localhost:9999") => "http://localhost:9999")
;; anthropic 兼容端点后缀要剥离
(check (pi-resolve-deepseek-search-base-url '() "https://api.deepseek.com/anthropic") => "https://api.deepseek.com")
(check (pi-resolve-deepseek-search-base-url '() #f '(("DEEPSEEK_BASE_URL" . "https://api.deepseek.com/anthropic"))) => "https://api.deepseek.com")
;; config 的 model-meta baseURL（provider 为 deepseek 时）
(check (pi-resolve-deepseek-search-base-url
         '((provider . "deepseek")
           (model-meta . (("baseURL" . "https://api.deepseek.com/anthropic")))) #f) => "https://api.deepseek.com")

(check-report)
