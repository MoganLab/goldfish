;; pi-model-entry-max-tokens
;; 读取 <pi-model-entry> 的 maxTokens 字段。
;;
;; 语法
;; ----
;; (pi-model-entry-max-tokens entry) -> number | #f

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

(check (pi-model-entry-max-tokens (make-pi-model-entry #f #f #f #f 'unset #f 32768))
  =>
  32768
) ;check
(check (pi-model-entry-max-tokens (make-pi-model-entry)) => #f)

(check-report)
