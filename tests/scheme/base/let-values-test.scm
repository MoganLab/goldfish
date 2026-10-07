(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; let-values
;; 并行绑定多值表达式的返回结果到变量。
;;
;; 语法
;; ----
;; (let-values ((<formals> <expression>) ...) <body> ...)
;;
;; 参数
;; ----
;; <formals> : (variable ...) | (variable ... . rest-variable) | rest-variable
;; 形参列表，可以为固定长度变量列表、以点号结尾的点对列表（带剩余变量），或单一标识符。
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
;; let-values 来源于 SRFI-11 与 R7RS (scheme base)。
;; 所有 <expression> 先在当前环境中求值，然后并行绑定到各 <formals> 变量上。
;;
;; 示例
;; ----
;; (let-values (((a b . c) (values 1 2 3 4)))
;;   (list a b c))
;; => (1 2 (3 4))
;;
;; (let ((a 'a) (b 'b) (x 'x) (y 'y))
;;   (let-values (((a b) (values x y))
;;                ((x y) (values a b)))
;;     (list a b x y)))
;; => (x y a b)

;; 空绑定测试
(check (let-values () 42) => 42)

;; 单值绑定
(check (let-values (((ret) (+ 1 2))) (+ ret 4)) => 7)

;; 多值绑定
(check (let-values (((a b) (values 3 4))) (+ a b)) => 7)

;; SRFI-11 官方用例：点对可变参数多值绑定
(check (let-values (((a b . c) (values 1 2 3 4)))
         (list a b c))
  => '(1 2 (3 4)))

;; SRFI-11 官方用例：并行求值与并行绑定语义验证
(check (let ((a 'a) (b 'b) (x 'x) (y 'y))
         (let-values (((a b) (values x y))
                      ((x y) (values a b)))
           (list a b x y)))
  => '(x y a b))

;; 单一变量捕获全部多值列表
(check (let-values ((args (values 1 2 3)))
         args)
  => '(1 2 3))

;; 多组绑定
(check (let-values (((x y) (values 1 2)) ((z w) (values 3 4)))
         (+ x y z w))
  => 10)

;; 嵌套使用
(check (let-values (((a b) (values 1 2)))
         (let-values (((c d) (values (+ a b) (* a b))))
           (+ c d)))
  => 5)

(check-report)
