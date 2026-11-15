;; pi-model-entry-id
;; 读取 <pi-model-entry> 的 id 字段。
;;
;; 语法
;; ----
;; (pi-model-entry-id entry) -> string | #f

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

(check (pi-model-entry-id (make-pi-model-entry "m1")) => "m1")
(check (pi-model-entry-id (make-pi-model-entry)) => #f)

(check-report)
