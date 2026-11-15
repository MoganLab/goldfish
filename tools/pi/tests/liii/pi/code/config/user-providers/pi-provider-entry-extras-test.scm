;; pi-provider-entry-extras
;; 读取 <pi-provider-entry> 的扩展字段 alist。
;;
;; 语法
;; ----
;; (pi-provider-entry-extras entry) -> alist

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

(check (pi-provider-entry-extras (make-pi-provider-entry #f #f #f #f #f #f '(("x" . 42))))
  =>
  '(("x" . 42))
) ;check
(check (pi-provider-entry-extras (make-pi-provider-entry)) => '())

(check-report)
