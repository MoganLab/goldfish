;; login-dialog-provider
;; 获取登录输入对话框当前关联的目标服务商标识。
;;
;; 语法
;; ----
;; (login-dialog-provider dialog)
;;
;; 参数
;; ----
;; dialog : login-dialog?
;; 登录对话框组件。
;;
;; 返回值
;; ----
;; string?
;; 服务商标识名称。

(import (scheme base)
  (liii check)
  (liii pi code modes interactive login-dialog)
) ;import

(check-set-mode! 'report-failed)

;; 1. 正确读取 provider 标识
(let ((dialog (make-login-dialog "kimi-coding" (lambda (k) #f) (lambda () #f))))
  (check (login-dialog-provider dialog) => "kimi-coding")
) ;let

(check-report)
