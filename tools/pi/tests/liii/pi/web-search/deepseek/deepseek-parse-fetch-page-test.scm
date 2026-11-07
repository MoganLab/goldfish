;; deepseek-parse-fetch-page
;; 解析 deepseek 抓取接口返回的页面数据。
;;
;; 语法
;; ----
;; (deepseek-parse-fetch-page json-data)
;;
;; 参数
;; ----
;; json-data : list?
;; 已解析的 JSON 数据。
;;
;; 返回值
;; ----
;; list?
;; 包含 title 与 content 字段的页面 alist。

(import (liii check)
        (liii json)
        (liii pi web-search deepseek)
) ;import

(check-set-mode! 'report-failed)

;; 解析 deepseek 抓取接口返回的页面数据。
(let ((page (deepseek-parse-fetch-page
              (string->json "{\"title\":\"Example Page\",\"content\":\"# Example\\nbody\"}"))))
  (check (cdr (assoc "title" page)) => "Example Page")
  (check (cdr (assoc "content" page)) => "# Example\nbody"))

(check-report)
