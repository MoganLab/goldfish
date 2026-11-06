;; uri-authority
;; 读取 URI 的完整 authority 网络位置字符串。
;;
;; 语法
;; ----
;; (uri-authority uri-obj)
;;
;; 参数
;; ----
;; uri-obj : uri?
;;   要读取的 URI 对象。
;;
;; 返回值
;; ----
;; string?
;;   返回包含认证信息、主机与端口的 authority。

(import (liii check) (liii uri record))

(check-set-mode! 'report-failed)

(define u (make-uri-raw "https" "user:pass@example.com:8443" "/" '() #f))
(check (uri-authority u) => "user:pass@example.com:8443")
(check (uri-authority (make-uri-raw "mailto" "" "user@example.com" '() #f))
  =>
  ""
) ;check

(check-report)
