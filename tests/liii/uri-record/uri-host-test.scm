(import (liii check)
        (liii uri-record)
) ;import

(check-set-mode! 'report-failed)

;; uri-host
;; 从 netloc 解析获取 host。
;;
;; 语法
;; ----
;; (uri-host uri-obj)
;;
;; 返回值
;; ----
;; string? 或 #f
;;   返回 host 字符串。

;; 简单 host
(define u1 (make-uri-raw "https" "example.com" "/" '() #f))
(check (uri-host u1) => "example.com")

;; 带端口的 host
(define u2 (make-uri-raw "https" "example.com:8080" "/" '() #f))
(check (uri-host u2) => "example.com")

;; 带 user 的 host
(define u3 (make-uri-raw "https" "user@example.com" "/" '() #f))
(check (uri-host u3) => "example.com")

;; 完整 netloc
(define u4 (make-uri-raw "https" "user:pass@example.com:8080" "/" '() #f))
(check (uri-host u4) => "example.com")

;; 空 netloc
(define u5 (make-uri-raw "https" "" "/" '() #f))
(check (uri-host u5) => #f)

(check-report)
