(import (liii check)
        (liii json)
        (liii pi web-search deepseek)
) ;import

(check-set-mode! 'report-failed)

;; deepseek-sources-from-results
(let* ((json-str "{\"query\":\"goldfish\",\"results\":[{\"title\":\"T1\",\"url\":\"https://u1\",\"snippet\":\"s1\",\"summary\":\"m1\"},{\"title\":\"T2\",\"url\":\"https://u2\",\"snippet\":\"s2\"}]}")
       (json-data (string->json json-str))
       (results (deepseek-parse-search-results json-data))
       (sources (deepseek-sources-from-results results)))
  (check (length sources) => 2)
  (check (cdr (assoc 'title (car sources))) => "T1")
  (check (cdr (assoc 'url (car sources))) => "https://u1"))

(check-report)
