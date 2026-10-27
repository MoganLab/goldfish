(import (liii check)
        (liii json)
        (liii pi web-search deepseek)
) ;import

(check-set-mode! 'report-failed)

;; deepseek-parse-search-results
(let* ((json-str "{\"query\":\"goldfish\",\"results\":[{\"title\":\"T1\",\"url\":\"https://u1\",\"snippet\":\"s1\",\"summary\":\"m1\"},{\"title\":\"T2\",\"url\":\"https://u2\",\"snippet\":\"s2\"}]}")
       (json-data (string->json json-str))
       (results (deepseek-parse-search-results json-data)))
  (check (length results) => 2)
  (check (cdr (assoc "title" (car results))) => "T1")
  ;; 无 summary 时回退到 snippet
  (check (cdr (assoc "summary" (cadr results))) => "s2"))

;; 空 results
(check (deepseek-parse-search-results (string->json "{\"results\":[]}")) => '())
(check (deepseek-parse-search-results (string->json "{}")) => '())

(check-report)
