(import (liii check)
        (liii pi web-search config)
) ;import

(check-set-mode! 'report-failed)

;; pi-web-search-config-model
(check (pi-web-search-config-model
         '(("provider" . "google") ("model" . "gemini-2.5-pro"))) => "gemini-2.5-pro")
(check (pi-web-search-config-model '(("provider" . "deepseek"))) => #f)

(check-report)
