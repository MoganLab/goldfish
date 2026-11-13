;; login-selector-cursor-position
;; 计算登录供应商选择器在当前搜索输入下的光标相对坐标 (行 列)。
;;
;; 语法
;; ----
;; (login-selector-cursor-position selector)
;;
;; 参数
;; ----
;; selector : login-selector?
;; 登录供应商选择器组件。
;;
;; 返回值
;; ----
;; list?
;; 长度为 2 的整数列表，格式为 (row col)。

(import (scheme base)
  (liii check)
  (liii pi code modes interactive login-selector)
) ;import

(check-set-mode! 'report-failed)

;; 1. 初始光标位于第 6 行第 2 列
(let ((selector (make-login-selector (lambda (p) #f) (lambda () #f))))
  (check (login-selector-cursor-position selector) => '(6 2))
) ;let

(check-report)
