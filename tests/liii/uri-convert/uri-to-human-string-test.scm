(import (liii check)
        (liii uri-record)
        (liii uri-convert)
) ;import

(check-set-mode! 'report-failed)

;; uri->human-string
;; 将 URI 转换为人类可读的字符串（隐藏敏感信息）。
;;
;; 语法
;; ----
;; (uri->human-string uri-obj)
;;
;; 返回值
;; ----
;; string?
;;   返回人类可读的 URI 字符串（不含密码等敏感信息）。

;; 简单 URI
(define u1 (make-uri-raw "https" "example.com" "/" '() #f))
(check (uri->human-string u1) => "https://example.com/")

;; 带 path
(define u2 (make-uri-raw "https" "example.com" "/path/to/file" '() #f))
(check (uri->human-string u2) => "https://example.com/path/to/file")

;; 带端口
(define u3 (make-uri-raw "https" "example.com:8443" "/api" '() #f))
(check (uri->human-string u3) => "https://example.com:8443/api")

;; 带用户但隐藏密码
(define u4 (make-uri-raw "https" "user:secret@host.com" "/" '() #f))
(check (uri->human-string u4) => "https://host.com/")

;; 带 query 但隐藏（人类可读版本通常不包含 query）
(define u5 (make-uri-raw "https" "example.com" "/search" '(("q" . "hello")) #f))
(check (uri->human-string u5) => "https://example.com/search")

;; 带 fragment 但隐藏
(define u6 (make-uri-raw "https" "example.com" "/docs" '() "section-1"))
(check (uri->human-string u6) => "https://example.com/docs")

;; 完整 URI（隐藏敏感信息）
(define u7 (make-uri-raw "https" "user:pass@host:8443" "/api" '(("id" . "123")) "frag"))
(check (uri->human-string u7) => "https://host:8443/api")

(check-report)
