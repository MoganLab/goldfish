;; uri-raw-authority
;; 读取 URI 记录中的原始 authority 字符串。
;;
;; 语法
;; ----
;; (uri-raw-authority uri-obj)
;;
;; 参数
;; ----
;; uri-obj : uri?
;;   要读取的 URI 对象。
;;
;; 返回值
;; ----
;; string?
;;   返回与 uri-netloc-raw 相同的 authority。

(import (liii check) (liii uri record))

(check-set-mode! 'report-failed)

(define u (make-uri-raw "https" "user:pass@example.com:8443" "/" '() #f))
(check (uri-raw-authority u) => "user:pass@example.com:8443")
(check (uri-raw-authority u) => (uri-netloc-raw u))

(check-report)
