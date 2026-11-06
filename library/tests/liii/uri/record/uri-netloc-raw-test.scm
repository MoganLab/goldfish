;; uri-netloc-raw
;; 读取 URI 记录中未经处理的 netloc 字段。
;;
;; 语法
;; ----
;; (uri-netloc-raw uri-obj)
;;
;; 参数
;; ----
;; uri-obj : uri?
;;   要读取的 URI 对象。
;;
;; 返回值
;; ----
;; string?
;;   返回包含认证信息、主机与端口的原始网络位置字符串。

(import (liii check) (liii uri record))

(check-set-mode! 'report-failed)

(define u (make-uri-raw "https" "user:pass@example.com:8443" "/" '() #f))

(define empty (make-uri-raw "mailto" "" "user@example.com" '() #f))
(check (uri-netloc-raw u) => "user:pass@example.com:8443")
(check (uri-netloc-raw empty) => "")

(check-report)
