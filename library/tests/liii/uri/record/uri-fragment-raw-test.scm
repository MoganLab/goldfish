;; uri-fragment-raw
;; 读取 URI 记录中未经处理的 fragment 字段。
;;
;; 语法
;; ----
;; (uri-fragment-raw uri-obj)
;;
;; 参数
;; ----
;; uri-obj : uri?
;;   要读取的 URI 对象。
;;
;; 返回值
;; ----
;; string? 或 #f
;;   返回构造记录时保存的片段标识符。

(import (liii check) (liii uri record))

(check-set-mode! 'report-failed)

(define u (make-uri-raw "https" "example.com" "/" '() "section 1"))
(check (uri-fragment-raw u) => "section 1")
(check (uri-fragment-raw (make-uri-raw "https" "example.com" "/" '() #f)) => #f)

(check-report)
