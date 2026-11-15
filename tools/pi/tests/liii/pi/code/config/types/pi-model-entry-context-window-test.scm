;; pi-model-entry-context-window
;; 读取 <pi-model-entry> 的 contextWindow 字段。
;;
;; 语法
;; ----
;; (pi-model-entry-context-window entry) -> number | #f

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

(check (pi-model-entry-context-window (make-pi-model-entry #f #f #f #f 'unset 128000))
  =>
  128000
) ;check
(check (pi-model-entry-context-window (make-pi-model-entry)) => #f)

(check-report)
