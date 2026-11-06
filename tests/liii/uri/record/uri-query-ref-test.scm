;; uri-query-ref
;; 获取指定 key 的 query 值。
;;
;; 语法
;; ----
;; (uri-query-ref uri-obj key)
;;
;; 参数
;; ----
;; uri-obj : uri?
;;   要读取的 URI 对象。
;; key : string
;;   查询参数名称。
;;
;; 返回值
;; ----
;; string? 或 #f
;;   返回 key 对应的值，不存在则返回 #f。

(import (liii check) (liii uri record))

(check-set-mode! 'report-failed)

(define u
  (make-uri-raw "https" "example.com" "/" '(("a" . "1") ("b" . "2")) #f)
) ;define


;; 存在的 key
(check (uri-query-ref u "a") => "1")
(check (uri-query-ref u "b") => "2")


;; 不存在的 key
(check (uri-query-ref u "c") => #f)


(check-report)
