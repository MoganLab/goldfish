;; uri-raw-host
;; 从 URI 的原始 netloc 字段读取主机名。
;;
;; 语法
;; ----
;; (uri-raw-host uri-obj)
;;
;; 参数
;; ----
;; uri-obj : uri?
;;   要读取的 URI 对象。
;;
;; 返回值
;; ----
;; string? 或 #f
;;   返回与 uri-host 相同的主机名。

(import (liii check) (liii uri record))

(check-set-mode! 'report-failed)

(define u (make-uri-raw "https" "user@example.com:8443" "/" '() #f))
(check (uri-raw-host u) => "example.com")
(check (uri-raw-host u) => (uri-host u))
(check (uri-raw-host (make-uri-raw "https" "" "/" '() #f)) => #f)

(check-report)
