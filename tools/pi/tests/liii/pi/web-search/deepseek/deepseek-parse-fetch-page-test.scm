(import (liii check)
        (liii json)
        (liii pi web-search deepseek)
) ;import

(check-set-mode! 'report-failed)

;; deepseek-parse-fetch-page
(let ((page (deepseek-parse-fetch-page
              (string->json "{\"title\":\"Example Page\",\"content\":\"# Example\\nbody\"}"))))
  (check (cdr (assoc "title" page)) => "Example Page")
  (check (cdr (assoc "content" page)) => "# Example\nbody"))

(check-report)
