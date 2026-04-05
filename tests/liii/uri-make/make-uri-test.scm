(import (liii check)
        (liii uri-record)
        (liii uri-make)
) ;import

(check-set-mode! 'report-failed)

;; make-uri
;; 从字符串解析构造 URI。
;;
;; 语法
;; ----
;; (make-uri str)
;;
;; 返回值
;; ----
;; uri?
;;   返回解析后的 URI 记录。

;; 简单 HTTP URI
(define u1 (make-uri "http://example.com/"))
(check (uri-scheme u1) => "http")
(check (uri-host u1) => "example.com")
(check (uri-path u1) => "/")

;; HTTPS URI
(define u2 (make-uri "https://example.com/path/to/file"))
(check (uri-scheme u2) => "https")
(check (uri-host u2) => "example.com")
(check (uri-path u2) => "/path/to/file")

;; 带端口的 URI
(define u3 (make-uri "http://example.com:8080/api"))
(check (uri-host u3) => "example.com")
(check (uri-explicit-port u3) => 8080)

;; 带查询字符串的 URI
(define u4 (make-uri "https://example.com/search?q=hello&page=1"))
(check (uri-path u4) => "/search")
(check (uri-query-ref u4 "q") => "hello")
(check (uri-query-ref u4 "page") => "1")

;; 带 fragment 的 URI
(define u5 (make-uri "https://example.com/docs#section-1"))
(check (uri-path u5) => "/docs")
(check (uri-fragment u5) => "section-1")

;; 完整 URI
(define u6 (make-uri "https://user:pass@host:8443/path?a=1#frag"))
(check (uri-scheme u6) => "https")
(check (uri-user u6) => "user")
(check (uri-password u6) => "pass")
(check (uri-host u6) => "host")
(check (uri-explicit-port u6) => 8443)
(check (uri-path u6) => "/path")
(check (uri-query-ref u6 "a") => "1")
(check (uri-fragment u6) => "frag")

;; Git SSH 格式
(define u7 (make-uri "git@github.com:liii/uri.git"))
(check (uri-scheme u7) => "ssh")
(check (uri-user u7) => "git")
(check (uri-host u7) => "github.com")
(check (uri-path u7) => ":liii/uri.git")

;; 错误处理
(check-catch 'type-error (make-uri 123))
(check-catch 'type-error (make-uri '()))

(check-report)
