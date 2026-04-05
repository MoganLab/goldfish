(import (liii check)
        (liii uri-record)
        (liii uri-convert)
) ;import

(check-set-mode! 'report-failed)

;; uri->string
;; 将 URI 转换为字符串。
;;
;; 语法
;; ----
;; (uri->string uri-obj)
;;
;; 返回值
;; ----
;; string?
;;   返回 URI 字符串。

;; 简单 URI
(define u1 (make-uri-raw "https" "example.com" "/" '() #f))
(check (uri->string u1) => "https://example.com/")

;; 带 path
(define u2 (make-uri-raw "https" "example.com" "/path/to/file" '() #f))
(check (uri->string u2) => "https://example.com/path/to/file")

;; 带 query
(define u3 (make-uri-raw "https" "example.com" "/search" '(("q" . "hello")) #f))
(check (uri->string u3) => "https://example.com/search?q=hello")

;; 带 fragment
(define u4 (make-uri-raw "https" "example.com" "/docs" '() "section-1"))
(check (uri->string u4) => "https://example.com/docs#section-1")

;; 完整 URI
(define u5 (make-uri-raw "https" "user:pass@host:8443" "/api" '(("id" . "123")) "frag"))
(check (uri->string u5) => "https://user:pass@host:8443/api?id=123#frag")

;; 相对 URI（无 scheme）
(define u6 (make-uri-raw #f "" "/path" '() #f))
(check (uri->string u6) => "/path")

(check-report)
