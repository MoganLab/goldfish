;; uri-query
;; 获取 URI 的 query（alist 格式）。
;;
;; 语法
;; ----
;; (uri-query uri-obj)
;;
;; 参数
;; ----
;; uri-obj : uri?
;;   要读取的 URI 对象。
;;
;; 返回值
;; ----
;; alist?
;;   返回查询参数的关联列表。

(import (liii check) (liii uri record))

(check-set-mode! 'report-failed)

;; 有 query

(define u1
  (make-uri-raw "https" "example.com" "/" '(("a" . "1") ("b" . "2")) #f)
) ;define
(check (uri-query u1) => '(("a" . "1") ("b" . "2")))


;; 空 query

(define u2 (make-uri-raw "https" "example.com" "/" '() #f))
(check (uri-query u2) => '())


(check-report)
