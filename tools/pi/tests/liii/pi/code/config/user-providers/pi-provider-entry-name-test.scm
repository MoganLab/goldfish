;; pi-provider-entry-name
;; 读取 <pi-provider-entry> 的 name 字段。
;;
;; 语法
;; ----
;; (pi-provider-entry-name entry) -> string | #f

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

(check (pi-provider-entry-name (make-pi-provider-entry #f "Kimi")) => "Kimi")
(check (pi-provider-entry-name (make-pi-provider-entry)) => #f)

(check-report)
