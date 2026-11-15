;; pi-model-entry-extras
;; 读取 <pi-model-entry> 的扩展字段 alist。
;;
;; 语法
;; ----
;; (pi-model-entry-extras entry) -> alist

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

(check (pi-model-entry-extras (make-pi-model-entry #f #f #f #f 'unset #f #f '(("k" . 1))))
  =>
  '(("k" . 1))
) ;check
(check (pi-model-entry-extras (make-pi-model-entry)) => '())

(check-report)
