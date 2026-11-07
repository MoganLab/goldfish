;; deepseek-format-search-text
;; 将 deepseek 搜索结果列表格式化为可读文本。
;;
;; 语法
;; ----
;; (deepseek-format-search-text results)
;;
;; 参数
;; ----
;; results : list?
;; deepseek 搜索结果条目列表。
;;
;; 返回值
;; ----
;; string?
;; 格式化后的搜索结果文本。

(import (liii check)
        (liii string)
        (liii json)
        (liii pi web-search deepseek)
) ;import

(check-set-mode! 'report-failed)

;; 格式化 deepseek 搜索结果为文本。
(let* ((json-str "{\"query\":\"goldfish\",\"results\":[{\"title\":\"T1\",\"url\":\"https://u1\",\"snippet\":\"s1\",\"summary\":\"m1\"},{\"title\":\"T2\",\"url\":\"https://u2\",\"snippet\":\"s2\"}]}")
       (json-data (string->json json-str))
       (results (deepseek-parse-search-results json-data))
       (text (deepseek-format-search-text results)))
  (check-true (string-contains? text "T1"))
  (check-true (string-contains? text "m1")))

(check-report)
