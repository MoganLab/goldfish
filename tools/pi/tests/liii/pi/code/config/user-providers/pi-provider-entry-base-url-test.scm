;; pi-provider-entry-base-url
;; 读取 <pi-provider-entry> 的 baseUrl 字段。
;;
;; 语法
;; ----
;; (pi-provider-entry-base-url entry) -> string | #f

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

(check (pi-provider-entry-base-url (make-pi-provider-entry #f #f "https://api.example.com"))
  =>
  "https://api.example.com"
) ;check
(check (pi-provider-entry-base-url (make-pi-provider-entry)) => #f)

(check-report)
