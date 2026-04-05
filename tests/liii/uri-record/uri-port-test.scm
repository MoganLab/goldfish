(import (liii check)
        (liii uri-record)
) ;import

(check-set-mode! 'report-failed)

;; uri-port
;; 获取 URI 的端口（优先显式端口，否则返回默认端口）。
;;
;; 语法
;; ----
;; (uri-port uri-obj)
;;
;; 返回值
;; ----
;; number? 或 #f
;;   返回端口号或 #f。

;; 显式端口
(define u1 (make-uri-raw "https" "example.com:8080" "/" '() #f))
(check (uri-port u1) => 8080)

;; 使用默认端口 (https = 443)
(define u2 (make-uri-raw "https" "example.com" "/" '() #f))
(check (uri-port u2) => 443)

;; 使用默认端口 (http = 80)
(define u3 (make-uri-raw "http" "example.com" "/" '() #f))
(check (uri-port u3) => 80)

;; 无 scheme，无端口
(define u4 (make-uri-raw #f "example.com" "/" '() #f))
(check (uri-port u4) => #f)

(check-report)
