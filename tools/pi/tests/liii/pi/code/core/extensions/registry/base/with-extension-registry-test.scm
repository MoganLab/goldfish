;; with-extension-registry
;; 在临时绑定注册表的作用域内执行过程，结束后恢复原状态。
;;
;; 语法
;; ----
;; (with-extension-registry target thunk)
;;
;; 参数
;; ----
;; target : (or extension-registry extension-api extension-context #f)
;; 目标对象。
;;
;; thunk : procedure
;; 无参过程。
;;
;; 返回值
;; ----
;; any
;; thunk 的返回值。

(import (scheme base)
  (liii check)
  (liii pi code core extensions registry base)
  (liii pi code core extensions types)
) ;import

(check-set-mode! 'report-failed)

(let* ((reg (make-extension-registry)) (saved (current-extension-registry)))
  (check (with-extension-registry reg (lambda () (current-extension-registry)))
    =>
    reg
  ) ;check
  (check (current-extension-registry) => saved)
) ;let*

(check-report)
