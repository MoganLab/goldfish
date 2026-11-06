;; string->uri
;; make-uri 的别名，从字符串解析构造 URI。
;;
;; 语法
;; ----
;; (string->uri str)
;;
;; 参数
;; ----
;; str : string
;;   要解析的 URI 字符串。
;;
;; 返回值
;; ----
;; uri?
;;   返回解析后的 URI 记录。

(import (liii check) (liii uri record) (liii uri make) (liii uri compare))

(check-set-mode! 'report-failed)

;; 与 make-uri 结果相同

(define u1 (string->uri "https://example.com/path"))

(define u2 (make-uri "https://example.com/path"))
(check (uri=? u1 u2) => #t)


;; 各种格式
(check (uri-scheme (string->uri "http://a.com")) => "http")
(check (uri-host (string->uri "https://b.com:8080")) => "b.com")
(check (uri-path (string->uri "ftp://c.com/files")) => "/files")


;; 简单用法

(define url "https://api.example.com/v1/users?id=123")

(define uri (string->uri url))
(check (uri-scheme uri) => "https")
(check (uri-host uri) => "api.example.com")
(check (uri-path uri) => "/v1/users")
(check (uri-query-ref uri "id") => "123")


(check-report)
