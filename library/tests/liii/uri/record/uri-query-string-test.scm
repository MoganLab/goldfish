;; uri-query-string
;; 将 URI 的查询参数 alist 转换为查询字符串。
;;
;; 语法
;; ----
;; (uri-query-string uri-obj)
;;
;; 参数
;; ----
;; uri-obj : uri?
;;   要读取的 URI 对象。
;;
;; 返回值
;; ----
;; string
;;   返回不带前导问号的查询字符串，空查询返回空字符串。

(import (liii check) (liii uri record))

(check-set-mode! 'report-failed)

(define u
  (make-uri-raw "https" "example.com" "/" '(("a" . "1") ("q" . "hello world")) #f)
) ;define
(check (uri-query-string u) => "a=1&q=hello%20world")
(check (uri-query-string (make-uri-raw "https" "example.com" "/" '() #f)) => "")

(check-report)
