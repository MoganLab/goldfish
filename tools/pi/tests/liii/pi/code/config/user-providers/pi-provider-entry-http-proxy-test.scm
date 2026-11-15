;; pi-provider-entry-http-proxy
;; 读取 <pi-provider-entry> 的 httpProxy 字段。
;;
;; 语法
;; ----
;; (pi-provider-entry-http-proxy entry) -> string | #f

;;
;; 参数
;; ----
;; entry : pi-provider-entry?
;;
;; 返回值
;; ----
;; any

(import (scheme base) (liii check) (liii pi code config user-providers))

(check-set-mode! 'report-failed)

(check (pi-provider-entry-http-proxy (make-pi-provider-entry #f #f #f #f "http://127.0.0.1:7890"))
  =>
  "http://127.0.0.1:7890"
) ;check
(check (pi-provider-entry-http-proxy (make-pi-provider-entry)) => #f)

(check-report)
