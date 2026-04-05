(import (liii check)
        (liii uri-record)
        (liii uri-transform)
) ;import

(check-set-mode! 'report-failed)

;; uri-with-scheme
;; 替换 URI 的 scheme。
;;
;; 语法
;; ----
;; (uri-with-scheme uri-obj new-scheme)
;;
;; 返回值
;; ----
;; uri?
;;   返回新的 URI 记录。

;; 修改 scheme
(define u1 (make-uri-raw "http" "example.com" "/" '() #f))
(define u2 (uri-with-scheme u1 "https"))
(check (uri-scheme u2) => "https")
(check (uri-host u2) => "example.com")
(check (uri-path u2) => "/")

;; 添加 scheme 到相对 URI
(define u3 (make-uri-raw #f "" "/path" '() #f))
(define u4 (uri-with-scheme u3 "https"))
(check (uri-scheme u4) => "https")
(check (uri-path u4) => "/path")

;; 其他字段保持不变
(define u5 (make-uri-raw "http" "api.com:8080" "/v1" '(("k" . "v")) "frag"))
(define u6 (uri-with-scheme u5 "https"))
(check (uri-scheme u6) => "https")
(check (uri-host u6) => "api.com")
(check (uri-explicit-port u6) => 8080)
(check (uri-path u6) => "/v1")
(check (uri-query-ref u6 "k") => "v")
(check (uri-fragment u6) => "frag")

(check-report)
