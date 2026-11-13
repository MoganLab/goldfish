;; login-selector?
;; 判断组件是否为登录供应商选择器。
;;
;; 语法
;; ----
;; (login-selector? obj)
;;
;; 参数
;; ----
;; obj : any?
;; 待判定的对象。
;;
;; 返回值
;; ----
;; boolean?
;; 若为 login-selector 组件则返回 #t，否则返回 #f。

(import (scheme base)
  (liii check)
  (liii pi code modes interactive login-selector)
) ;import

(check-set-mode! 'report-failed)

;; 1. 正确判定 login-selector 组件
(let ((selector (make-login-selector (lambda (p) #f) (lambda () #f))))
  (check-true (login-selector? selector))
  (check-false (login-selector? 123))
  (check-false (login-selector? "not-a-selector"))
  (check-false (login-selector? '()))
) ;let

(check-report)
