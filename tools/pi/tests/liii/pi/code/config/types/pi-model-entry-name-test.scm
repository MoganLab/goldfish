;; pi-model-entry-name
;; 读取 <pi-model-entry> 的 name 字段。
;;
;; 语法
;; ----
;; (pi-model-entry-name entry) -> string | #f

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

(check (pi-model-entry-name (make-pi-model-entry #f "Model 1")) => "Model 1")
(check (pi-model-entry-name (make-pi-model-entry)) => #f)

(check-report)
