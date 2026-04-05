(import (liii check)
        (liii uri-record)
        (liii uri-transform)
) ;import

(check-set-mode! 'report-failed)

;; uri-without-query
;; 移除 URI 的所有 query 参数。
;;
;; 语法
;; ----
;; (uri-without-query uri-obj)
;;
;; 返回值
;; ----
;; uri?
;;   返回新的 URI 记录。

;; 移除所有 query
(define u1 (make-uri-raw "https" "api.com" "/" '(("a" . "1") ("b" . "2")) #f))
(define u2 (uri-without-query u1))
(check (uri-query-raw u2) => '())
(check (uri-query-ref u2 "a") => #f)

;; 空 query 保持不变
(define u3 (make-uri-raw "https" "api.com" "/" '() #f))
(define u4 (uri-without-query u3))
(check (uri-query-raw u4) => '())

;; 保留其他字段
(define u5 (make-uri-raw "https" "api.com" "/path" '(("k" . "v")) "frag"))
(define u6 (uri-without-query u5))
(check (uri-path u6) => "/path")
(check (uri-fragment u6) => "frag")

(check-report)
