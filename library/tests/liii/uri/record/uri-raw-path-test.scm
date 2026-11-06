;; uri-raw-path
;; 读取 URI 的原始路径字符串。
;;
;; 语法
;; ----
;; (uri-raw-path uri-obj)
;;
;; 参数
;; ----
;; uri-obj : uri?
;;   要读取的 URI 对象。
;;
;; 返回值
;; ----
;; string? 或 #f
;;   返回与 uri-path-raw 相同的路径值。

(import (liii check) (liii uri record))

(check-set-mode! 'report-failed)

(define u (make-uri-raw "https" "example.com" "/a b" '() #f))
(check (uri-raw-path u) => "/a b")
(check (uri-raw-path u) => (uri-path-raw u))
(check (uri-raw-path (make-uri-raw "https" "example.com" "" '() #f)) => "")

(check-report)
