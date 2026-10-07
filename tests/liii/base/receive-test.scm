(import (liii check) (liii base))

(check-set-mode! 'report-failed)

;; receive
;; 将多值表达式的返回结果绑定到形参列表并执行表达式体。
;;
;; 语法
;; ----
;; (receive <formals> <expression> <body> ...)
;;
;; 参数
;; ----
;; <formals> : (variable ...) | (variable ... . rest-variable) | rest-variable
;; 形参列表，可以为固定长度变量列表、以点号结尾的点对列表（带剩余变量），或捕获全部值的单一标识符。
;;
;; <expression> : expression
;; 返回多值的表达式（如通过 values 返回）。
;;
;; <body> ... : expression ...
;; 表达式体。
;;
;; 返回值
;; -----
;; any
;; 返回最后一个 body 表达式的求值结果。
;;
;; 说明
;; ----
;; receive 宏来源于 SRFI-8。
;; 它为 call-with-values 提供了更简洁、可读性更高的接收多返回值语法抽象。
;;
;; 示例
;; ----
;; (receive (a b) (values 1 2)
;;   (+ a b))
;; => 3
;;
;; (receive (a b . c) (values 1 2 3 4)
;;   (list a b c))
;; => (1 2 (3 4))
;;
;; (receive all (values 1 2 3)
;;   all)
;; => (1 2 3)

;; 1. 固定长度多值参数绑定
(check (receive (a b) (values 1 2) (+ a b)) => 3)
(check (receive (a b c) (values 1 2 3) (list a b c)) => '(1 2 3))

;; 2. 点对变长多值参数绑定
(check (receive (a b . c) (values 1 2 3 4) (list a b c)) => '(1 2 (3 4)))
(check (receive (a . rest) (values 1 2 3) (cons a rest)) => '(1 2 3))
(check (receive (a . rest) (values 1) (cons a rest)) => '(1))

;; 3. 单变量捕获全部多值列表
(check (receive all (values 1 2 3) all) => '(1 2 3))

;; 4. 单值表达式接收
(check (receive (x) (+ 1 2) (* x 2)) => 6)

;; 5. 多 body 表达式顺序执行
(check (receive (a b) (values 10 20)
         (+ a 1)
         (+ a b))
  => 30)

;; 6. 参数数目不匹配错误测试
(check-catch 'wrong-number-of-args (receive (a b) (values 1) (+ a b)))
(check-catch 'wrong-number-of-args (receive (a b) (values 1 2 3) (+ a b)))

(check-report)
