;; login-dialog?
;; 判断组件是否为 API Key 登录输入对话框。
;;
;; 语法
;; ----
;; (login-dialog? obj)
;;
;; 参数
;; ----
;; obj : any?
;; 待判定的对象。
;;
;; 返回值
;; ----
;; boolean?
;; 若为 login-dialog 组件则返回 #t，否则返回 #f。

(import (scheme base)
  (liii check)
  (liii pi code modes interactive login-dialog)
) ;import

(check-set-mode! 'report-failed)

;; 1. 正确判定 login-dialog 组件
(let ((dialog (make-login-dialog "deepseek" (lambda (k) #f) (lambda () #f))))
  (check-true (login-dialog? dialog))
  (check-false (login-dialog? "not-a-dialog"))
  (check-false (login-dialog? 42))
) ;let

(check-report)
