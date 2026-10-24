(import (liii check) (liii uri record) (liii uri predicate))


(check-set-mode! 'report-failed)


;; uri-default-port?
;; 检查 URI 实际使用的端口是否为协议默认端口。
;;
;; 语法
;; ----
;; (uri-default-port? uri-obj)
;;
;; 返回值
;; ----
;; boolean?
;;   协议为 http/https/ftp 等有默认端口的网络协议，且无显式端口
;;   （隐式使用默认端口）或显式端口等于默认端口时返回 #t；
;;   其余情况返回 #f。


;; 无显式端口的 https URI，隐式使用默认端口 443

(define u1 (make-uri-raw "https" "example.com" "/" '() #f))
(check (uri-default-port? u1) => #t)


;; 显式端口等于默认端口

(define u2 (make-uri-raw "https" "example.com:443" "/" '() #f))
(check (uri-default-port? u2) => #t)


;; 显式端口不等于默认端口

(define u3 (make-uri-raw "https" "example.com:8443" "/" '() #f))
(check (uri-default-port? u3) => #f)


;; 无显式端口的 http URI，隐式使用默认端口 80

(define u4 (make-uri-raw "http" "example.com" "/" '() #f))
(check (uri-default-port? u4) => #t)


;; 显式端口 8080 不等于默认端口 80

(define u5 (make-uri-raw "http" "example.com:8080" "/" '() #f))
(check (uri-default-port? u5) => #f)


;; 非网络协议（如 mailto），无默认端口

(define u6 (make-uri-raw "mailto" "" "/a" '() #f))
(check (uri-default-port? u6) => #f)


(check-report)
