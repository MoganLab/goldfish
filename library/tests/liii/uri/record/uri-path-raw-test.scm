;; uri-path-raw
;; 读取 URI 记录中未经处理的 path 字段。
;;
;; 语法
;; ----
;; (uri-path-raw uri-obj)
;;
;; 参数
;; ----
;; uri-obj : uri?
;;   要读取的 URI 对象。
;;
;; 返回值
;; ----
;; string? 或 #f
;;   返回构造记录时保存的路径值。

(import (liii check) (liii uri record))

(check-set-mode! 'report-failed)

(define u (make-uri-raw "https" "example.com" "/a b" '() #f))
(check (uri-path-raw u) => "/a b")
(check (uri-path-raw (make-uri-raw "https" "example.com" "" '() #f)) => "")

(check-report)
