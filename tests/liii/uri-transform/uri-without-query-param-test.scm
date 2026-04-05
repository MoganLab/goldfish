(import (liii check)
        (liii uri-record)
        (liii uri-transform)
) ;import

(check-set-mode! 'report-failed)

;; uri-without-query-param
;; 移除 URI 的指定 query 参数。
;;
;; 语法
;; ----
;; (uri-without-query-param uri-obj key)
;;
;; 返回值
;; ----
;; uri?
;;   返回新的 URI 记录。

;; 移除单个参数
(define u1 (make-uri-raw "https" "api.com" "/" '(("a" . "1") ("b" . "2")) #f))
(define u2 (uri-without-query-param u1 "a"))
(check (uri-query-ref u2 "a") => #f)
(check (uri-query-ref u2 "b") => "2")

;; 移除不存在的参数（无变化）
(define u3 (make-uri-raw "https" "api.com" "/" '(("a" . "1")) #f))
(define u4 (uri-without-query-param u3 "nonexistent"))
(check (uri-query-ref u4 "a") => "1")

;; 空 query 保持不变
(define u5 (make-uri-raw "https" "api.com" "/" '() #f))
(define u6 (uri-without-query-param u5 "key"))
(check (uri-query-raw u6) => '())

(check-report)
