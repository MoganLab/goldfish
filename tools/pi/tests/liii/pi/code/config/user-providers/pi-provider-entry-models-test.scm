;; pi-provider-entry-models
;; 读取 <pi-provider-entry> 的 models 白名单字段。
;;
;; 语法
;; ----
;; (pi-provider-entry-models entry) -> list | #f

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

(check (pi-provider-entry-models (make-pi-provider-entry #f #f #f #f #f '("m1" "m2")))
  =>
  '("m1" "m2")
) ;check
(check (pi-provider-entry-models (make-pi-provider-entry)) => #f)

(check-report)
