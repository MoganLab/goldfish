;; deepseek-sources-from-results
;; 从 deepseek 搜索结果中提取来源列表。
;;
;; 语法
;; ----
;; (deepseek-sources-from-results results)
;;
;; 参数
;; ----
;; results : list?
;; deepseek 搜索结果条目列表。
;;
;; 返回值
;; ----
;; list?
;; 来源条目列表，每项包含标题与链接。

(import (liii check)
        (liii json)
        (liii pi web-search deepseek)
) ;import

(check-set-mode! 'report-failed)

;; 从 deepseek 搜索结果中提取来源列表。
(let* ((json-str "{\"query\":\"goldfish\",\"results\":[{\"title\":\"T1\",\"url\":\"https://u1\",\"snippet\":\"s1\",\"summary\":\"m1\"},{\"title\":\"T2\",\"url\":\"https://u2\",\"snippet\":\"s2\"}]}")
       (json-data (string->json json-str))
       (results (deepseek-parse-search-results json-data))
       (sources (deepseek-sources-from-results results)))
  (check (length sources) => 2)
  (check (cdr (assoc 'title (car sources))) => "T1")
  (check (cdr (assoc 'url (car sources))) => "https://u1"))

(check-report)
