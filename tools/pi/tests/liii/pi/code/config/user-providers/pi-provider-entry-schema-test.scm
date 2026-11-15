;; pi-provider-entry-schema
;; 读取 <pi-provider-entry> 的 $schema 字段。
;;
;; 语法
;; ----
;; (pi-provider-entry-schema entry) -> string | #f

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

(check (pi-provider-entry-schema (make-pi-provider-entry "https://schema.example"))
  =>
  "https://schema.example"
) ;check
(check (pi-provider-entry-schema (make-pi-provider-entry)) => #f)

(check-report)
