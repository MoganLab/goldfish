(import (liii check)
        (liii pi web-search deepseek)
) ;import

(check-set-mode! 'report-failed)

;; deepseek-build-search-request-body
(check (deepseek-build-search-request-body "hello") => '(("query" . "hello") ("count" . 10)))

(check-report)
