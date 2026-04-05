(import (liii check)
        (liii uri-record)
        (liii uri-transform)
) ;import

(check-set-mode! 'report-failed)

;; uri-with-port
;; 替换 URI 的 port。
;;
;; 语法
;; ----
;; (uri-with-port uri-obj new-port)
;;
;; 返回值
;; ----
;; uri?
;;   返回新的 URI 记录。

;; 修改 port
(define u1 (make-uri-raw "https" "example.com" "/" '() #f))
(define u2 (uri-with-port u1 8443))
(check (uri-explicit-port u2) => 8443)
(check (uri-host u2) => "example.com")

;; 保留其他 netloc 组件
(define u3 (make-uri-raw "https" "user@example.com:8080" "/" '() #f))
(define u4 (uri-with-port u3 9090))
(check (uri-user u4) => "user")
(check (uri-host u4) => "example.com")
(check (uri-explicit-port u4) => 9090)

;; 移除 port（设置为 #f）
(define u5 (make-uri-raw "http" "example.com:8080" "/" '() #f))
(define u6 (uri-with-port u5 #f))
(check (uri-explicit-port u6) => #f)

(check-report)
