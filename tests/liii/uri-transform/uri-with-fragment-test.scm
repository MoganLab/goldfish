(import (liii check)
        (liii uri-record)
        (liii uri-transform)
) ;import

(check-set-mode! 'report-failed)

;; uri-with-fragment
;; 替换 URI 的 fragment。
;;
;; 语法
;; ----
;; (uri-with-fragment uri-obj new-fragment)
;;
;; 返回值
;; ----
;; uri?
;;   返回新的 URI 记录。

;; 添加 fragment
(define u1 (make-uri-raw "https" "example.com" "/" '() #f))
(define u2 (uri-with-fragment u1 "section-1"))
(check (uri-fragment u2) => "section-1")

;; 修改 fragment
(define u3 (make-uri-raw "https" "example.com" "/" '() "old"))
(define u4 (uri-with-fragment u3 "new"))
(check (uri-fragment u4) => "new")

;; 保留其他字段
(define u5 (make-uri-raw "https" "api.com" "/v1" '(("k" . "v")) "old"))
(define u6 (uri-with-fragment u5 "section-2"))
(check (uri-path u6) => "/v1")
(check (uri-query-ref u6 "k") => "v")
(check (uri-fragment u6) => "section-2")

(check-report)
