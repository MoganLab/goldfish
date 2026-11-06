;; uri-netloc-set!
;; 原地修改 URI 记录的 netloc 字段。
;;
;; 语法
;; ----
;; (uri-netloc-set! uri-obj value)
;;
;; 参数
;; ----
;; uri-obj : uri?
;;   要修改的 URI 记录。
;; value : string?
;;   新的网络位置字符串。
;;
;; 返回值
;; ----
;; unspecified
;;   修改记录后的返回值未指定，应通过访问器检查结果。

(import (liii check) (liii uri record))

(check-set-mode! 'report-failed)

(define u (make-uri-raw "https" "old.example.com" "/" '() #f))
(uri-netloc-set! u "user@example.com:8443")
(check (uri-netloc-raw u) => "user@example.com:8443")
(check (uri-host u) => "example.com")
(check (uri-explicit-port u) => 8443)
(check (uri-path-raw u) => "/")

(check-report)
