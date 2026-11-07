;; deepseek-parse-search-results
;; 解析 deepseek 搜索接口返回的 JSON 结果数据。
;;
;; 语法
;; ----
;; (deepseek-parse-search-results json-data)
;;
;; 参数
;; ----
;; json-data : list?
;; 已解析的 JSON 数据。
;;
;; 返回值
;; ----
;; list?
;; 搜索结果条目列表。

(import (liii check)
        (liii json)
        (liii pi web-search deepseek)
) ;import

(check-set-mode! 'report-failed)

;; 解析 deepseek 搜索接口返回的 JSON 结果数据。
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
