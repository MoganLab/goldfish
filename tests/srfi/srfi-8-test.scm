;; (srfi srfi-8) 模块文档
;;
;; SRFI-8: receive: Binding to multiple values
;; 提供了将多值表达式的返回结果绑定到局部变量的 receive 宏。

(import (liii check) (srfi srfi-8))

(check-set-mode! 'report-failed)

;; 基础多值绑定
(check (receive (a b) (values 1 2) (+ a b)) => 3)

;; 点对多值绑定
(check (receive (head . tail) (values 1 2 3) (cons head tail)) => '(1 2 3))

;; 单变量捕获
(check (receive args (values 10 20) args) => '(10 20))

(check-report)
