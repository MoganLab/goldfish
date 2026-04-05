(import (liii check)
        (liii uri-record)
) ;import

(check-set-mode! 'report-failed)

;; uri-query-ref
;; 获取指定 key 的 query 值。
;;
;; 语法
;; ----
;; (uri-query-ref uri-obj key)
;;
;; 返回值
;; ----
;; string? 或 #f
;;   返回 key 对应的值，不存在则返回 #f。

(define u (make-uri-raw "https" "example.com" "/" '(("a" . "1") ("b" . "2")) #f))

;; 存在的 key
(check (uri-query-ref u "a") => "1")
(check (uri-query-ref u "b") => "2")

;; 不存在的 key
(check (uri-query-ref u "c") => #f)

(check-report)
