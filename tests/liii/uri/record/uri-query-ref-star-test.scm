;; uri-query-ref*
;; 获取 query 值，可指定默认值。
;;
;; 语法
;; ----
;; (uri-query-ref* uri-obj key [default])
;;
;; 参数
;; ----
;; uri-obj : uri?
;;   要读取的 URI 对象。
;; key : string
;;   查询参数名称。
;; default : any
;;   可选的默认值，缺省为 #f。
;;
;; 返回值
;; ----
;; string? 或 default
;;   返回 key 对应的值，不存在则返回 default（默认 #f）。

(import (liii check) (liii uri record))

(check-set-mode! 'report-failed)

(define u (make-uri-raw "https" "example.com" "/" '(("a" . "1")) #f))


;; 存在的 key
(check (uri-query-ref* u "a") => "1")


;; 不存在的 key，使用默认 #f
(check (uri-query-ref* u "b") => #f)


;; 不存在的 key，使用自定义默认值
(check (uri-query-ref* u "b" "default") => "default")
(check (uri-query-ref* u "b" "") => "")


(check-report)
