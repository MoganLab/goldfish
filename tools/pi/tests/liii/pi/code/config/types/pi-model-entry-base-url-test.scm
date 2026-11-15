;; pi-model-entry-base-url
;; 读取 <pi-model-entry> 的 baseUrl 字段。
;;
;; 语法
;; ----
;; (pi-model-entry-base-url entry) -> string | #f

;;
;; 参数
;; ----
;; entry : pi-model-entry?
;;
;; 返回值
;; ----
;; any

(import (scheme base) (liii check) (liii pi code config types))

(check-set-mode! 'report-failed)

(check (pi-model-entry-base-url (make-pi-model-entry #f #f "https://api.example.com"))
  =>
  "https://api.example.com"
) ;check
(check (pi-model-entry-base-url (make-pi-model-entry)) => #f)

(check-report)
