;; target->registry
;; 从扩展 API、上下文或注册表记录中解析出注册表记录。
;;
;; 语法
;; ----
;; (target->registry target)
;;
;; 参数
;; ----
;; target : (or extension-registry extension-api extension-context)
;; 目标对象。
;;
;; 返回值
;; ----
;; extension-registry
;; 解析出的注册表记录。

(import (scheme base)
        (liii check)
        (liii pi code core extensions registry base)
        (liii pi code core extensions types)
) ;import

(check-set-mode! 'report-failed)

(let ((reg (make-extension-registry)))
  (check (target->registry reg) => reg)
  (check (target->registry (make-extension-api reg)) => reg)
  (check (target->registry (make-extension-context #f "." #f reg)) => reg)
) ;let*

(check-catch 'type-error (target->registry "not-a-registry"))

(check-report)
