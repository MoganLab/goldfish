;; uri-explicit-port
;; 读取 URI 网络位置中显式指定的端口。
;;
;; 语法
;; ----
;; (uri-explicit-port uri-obj)
;;
;; 参数
;; ----
;; uri-obj : uri?
;;   要读取的 URI 对象。
;;
;; 返回值
;; ----
;; integer? 或 #f
;;   返回显式端口号；未指定时不回退到协议默认端口。

(import (liii check) (liii uri record))

(check-set-mode! 'report-failed)

(define explicit (make-uri-raw "https" "example.com:8443" "/" '() #f))

(define implicit (make-uri-raw "https" "example.com" "/" '() #f))
(check (uri-explicit-port explicit) => 8443)
(check (uri-explicit-port implicit) => #f)

(check-report)
