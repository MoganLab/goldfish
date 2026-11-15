;; pi-model-entry-reasoning
;; 读取 <pi-model-entry> 的 reasoning 字段。
;;
;; 语法
;; ----
;; (pi-model-entry-reasoning entry) -> boolean | 'unset

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

(check (pi-model-entry-reasoning (make-pi-model-entry #f #f #f #f #t)) => #t)
(check (pi-model-entry-reasoning (make-pi-model-entry #f #f #f #f #f)) => #f)
(check (pi-model-entry-reasoning (make-pi-model-entry)) => 'unset)

(check-report)
