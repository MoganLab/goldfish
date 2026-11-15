;; pi-provider-entry-api
;; 读取 <pi-provider-entry> 的 api 字段。
;;
;; 语法
;; ----
;; (pi-provider-entry-api entry) -> string | #f

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

(check (pi-provider-entry-api (make-pi-provider-entry #f #f #f "anthropic"))
  =>
  "anthropic"
) ;check
(check (pi-provider-entry-api (make-pi-provider-entry)) => #f)

(check-report)
