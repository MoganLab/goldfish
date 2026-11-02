;; component?
;; 判断给定对象是否为合法的基础组件实例。
;;
;; 语法
;; ----
;; (component? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待检查的对象。
;;
;; 返回值
;; ----
;; boolean?
;; 如果为组件实例返回 #t，否则返回 #f。

(import (scheme base)
  (liii check)
  (liii tui component)
) ;import

(check-set-mode! 'report-failed)

(check-true (component? (make-text "hello")))
(check-true (component? (make-spacer 1)))
(check-true (component? (make-component :render (lambda (w) '()))))
(check-false (component? "not-a-component"))
(check-false (component? 123))
(check-false (component? '()))

(check-report)
