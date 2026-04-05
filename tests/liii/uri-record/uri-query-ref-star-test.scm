(import (liii check)
        (liii uri-record)
) ;import

(check-set-mode! 'report-failed)

;; uri-query-ref*
;; 获取 query 值，可指定默认值。
;;
;; 语法
;; ----
;; (uri-query-ref* uri-obj key [default])
;;
;; 返回值
;; ----
;; string? 或 default
;;   返回 key 对应的值，不存在则返回 default（默认 #f）。

(define u (make-uri-raw "https" "example.com" "/" '(("a" . "1")) #f))

;; 存在的 key
(check (uri-query-ref* u "a") => "1")

;; 不存在的 key，使用默认 #f
(check (uri-query-ref* u "b") => #f)

;; 不存在的 key，使用自定义默认值
(check (uri-query-ref* u "b" "default") => "default")
(check (uri-query-ref* u "b" "") => "")

(check-report)
