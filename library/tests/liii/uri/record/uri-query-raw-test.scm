;; uri-query-raw
;; 读取 URI 记录中未经处理的 query 字段。
;;
;; 语法
;; ----
;; (uri-query-raw uri-obj)
;;
;; 参数
;; ----
;; uri-obj : uri?
;;   要读取的 URI 对象。
;;
;; 返回值
;; ----
;; alist?
;;   返回构造记录时保存的查询参数关联列表。

(import (liii check) (liii uri record))

(check-set-mode! 'report-failed)

(define query '(("a" . "1") ("b" . "hello world")))

(define u (make-uri-raw "https" "example.com" "/" query #f))
(check (uri-query-raw u) => query)
(check (uri-query-raw (make-uri-raw "https" "example.com" "/" '() #f)) => '())

(check-report)
