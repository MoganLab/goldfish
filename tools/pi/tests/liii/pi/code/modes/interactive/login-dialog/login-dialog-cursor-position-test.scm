;; login-dialog-cursor-position
;; 计算登录输入对话框当前的光标相对坐标 (行 列)。
;;
;; 语法
;; ----
;; (login-dialog-cursor-position dialog)
;;
;; 参数
;; ----
;; dialog : login-dialog?
;; 登录对话框组件。
;;
;; 返回值
;; ----
;; list?
;; 长度为 2 的整数列表，格式为 (row col)。

(import (scheme base)
  (liii check)
  (liii pi code modes interactive login-dialog)
) ;import

(check-set-mode! 'report-failed)

;; 1. 初始光标位于第 6 行第 2 列
(let ((dialog (make-login-dialog "deepseek" (lambda (k) #f) (lambda () #f))))
  (check (login-dialog-cursor-position dialog) => '(6 2))
) ;let

(check-report)
