;; mouse-region-child
;; 获取 mouse-region 包装组件内部所包含的原始子组件。
;;
;; 语法
;; ----
;; (mouse-region-child comp)
;;
;; 参数
;; ----
;; comp : mouse-region?
;; mouse-region 包装组件实例。
;;
;; 返回值
;; ----
;; component?
;; 原始子组件实例。

(import (scheme base)
  (liii check)
  (liii tui component)
  (liii tui box mouse-region)
) ;import

(check-set-mode! 'report-failed)

(let* ((child (make-text "inner")) (mr (make-mouse-region child (lambda (ev) #f))))
  (check (mouse-region-child mr) => child)
) ;let*

(check-catch 'type-error (mouse-region-child "not-a-mr"))

(check-report)
