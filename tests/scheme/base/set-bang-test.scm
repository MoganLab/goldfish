(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; set!
;; 为变量赋新值。
;;
;; 语法
;; ----
;; (set! variable expression)
;;
;; 参数
;; ----
;; variable : symbol?
;; 已绑定的变量名。
;; expression : 任意类型
;; 新值表达式。
;;
;; 返回值
;; ------
;; 未指定
;;
;; 说明
;; ----
;; 1. 变量必须先定义
;; 2. 用于修改变量状态
(let ((x 10))
  (set! x 20)
  (check x => 20)
) ;let
(let ((counter 0))
  (set! counter (+ counter 1))
  (set! counter (+ counter 1))
  (check counter => 2)
) ;let

(check-report)
