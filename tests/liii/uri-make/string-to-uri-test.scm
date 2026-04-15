(import (liii check)
  (liii uri-record)
  (liii uri-make)
  (liii uri-compare)
) ;import


(check-set-mode! 'report-failed)


;; string->uri
;; make-uri 的别名，从字符串解析构造 URI。
;;
;; 语法
;; ----
;; (string->uri str)
;;
;; 返回值
;; ----
;; uri?
;;   返回解析后的 URI 记录。


;; 与 make-uri 结果相同
(define u1
  (string->uri "https://example.com/path")
) ;define
(define u2
  (make-uri "https://example.com/path")
) ;define
(check (uri=? u1 u2) => #t)


;; 各种格式
(check (uri-scheme (string->uri "http://a.com")
       ) ;uri-scheme
  =>
  "http"
) ;check
(check (uri-host (string->uri "https://b.com:8080")
       ) ;uri-host
  =>
  "b.com"
) ;check
(check (uri-path (string->uri "ftp://c.com/files")
       ) ;uri-path
  =>
  "/files"
) ;check


;; 简单用法
(define url
  "https://api.example.com/v1/users?id=123"
) ;define
(define uri (string->uri url))
(check (uri-scheme uri) => "https")
(check (uri-host uri)
  =>
  "api.example.com"
) ;check
(check (uri-path uri) => "/v1/users")
(check (uri-query-ref uri "id")
  =>
  "123"
) ;check


(check-report)
