;; uri-query-set!
;; 原地修改 URI 记录的 query 字段。
;;
;; 语法
;; ----
;; (uri-query-set! uri-obj value)
;;
;; 参数
;; ----
;; uri-obj : uri?
;;   要修改的 URI 记录。
;; value : alist?
;;   新的查询参数关联列表。
;;
;; 返回值
;; ----
;; unspecified
;;   修改记录后的返回值未指定，应通过访问器检查结果。

(import (liii check) (liii uri record))

(check-set-mode! 'report-failed)

(define u (make-uri-raw "https" "example.com" "/" '() #f))
(uri-query-set! u '(("page" . "2")))
(check (uri-query-raw u) => '(("page" . "2")))
(check (uri-query-ref u "page") => "2")
(uri-query-set! u '())
(check (uri-query-raw u) => '())

(check-report)
