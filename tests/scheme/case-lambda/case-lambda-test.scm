(import (liii check) (scheme case-lambda))

(check-set-mode! 'report-failed)

;; case-lambda
;; 基于参数数量分派执行的变长参数过程构造宏。
;;
;; 语法
;; ----
;; (case-lambda <clause> ...) → procedure
;;
;; 其中每个 <clause> 的形式为：
;; (<formals> <body> ...)
;;
;; 参数
;; ----
;; <formals> : (variable ...) | (variable ... . rest-variable) | rest-variable
;; 形参列表，可以为固定长度列表、以点号结尾的点对列表（带有剩余参数），或捕获全部参数的单一标识符。
;;
;; <body> ... : expression ...
;; 匹配成功时执行的表达式序列。
;;
;; 返回值
;; -----
;; procedure
;; 返回一个变长参数过程。当该过程被调用时，会按顺序寻找第一个形参与实参相符的子句并执行；若所有子句均无法匹配，则抛出 wrong-number-of-args 错误。
;;
;; 说明
;; ----
;; case-lambda 宏来源于 SRFI-16 与 R7RS (scheme case-lambda)。
;; 它提供了基于实参个数的分派机制，常用于实现可选参数、参数默认值或多态重载。
;;
;; 示例
;; ----
;; (define plus
;;   (case-lambda
;;     (() 0)
;;     ((x) x)
;;     ((x y) (+ x y))
;;     ((x y z) (+ (+ x y) z))
;;     (args (apply + args))))
;; (plus)         => 0
;; (plus 1)       => 1
;; (plus 1 2)     => 3
;; (plus 1 2 3)   => 6
;; (plus 1 2 3 4) => 10

;; 1. SRFI-16 规范官方用例
(define plus
  (case-lambda
    (() 0)
    ((x) x)
    ((x y) (+ x y))
    ((x y z) (+ (+ x y) z))
    (args (apply + args))))

(check (plus) => 0)
(check (plus 1) => 1)
(check (plus 1 2) => 3)
(check (plus 1 2 3) => 6)
(check (plus 1 2 3 4) => 10)

;; 2. 基本用法与参数默认值模拟
(define make-range
  (case-lambda
    ((end) (make-range 0 end 1))
    ((start end) (make-range start end 1))
    ((start end step)
     (if (>= start end)
         '()
         (cons start (make-range (+ start step) end step))))))

(check (make-range 3) => '(0 1 2))
(check (make-range 1 4) => '(1 2 3))
(check (make-range 0 6 2) => '(0 2 4))

;; 3. 点对可变参数匹配与顺序分派测试
(define multi-improper
  (case-lambda
    ((x y z . rest) 'three-or-more)
    ((x . rest) 'one-or-more)))

(check (multi-improper 1) => 'one-or-more)
(check (multi-improper 1 2) => 'one-or-more)
(check (multi-improper 1 2 3) => 'three-or-more)
(check (multi-improper 1 2 3 4) => 'three-or-more)

;; 4. 错误处理测试
(check-catch 'wrong-number-of-args ((case-lambda ((a) a) ((a b) (* a b))) 1 2 3))
(check-catch 'wrong-number-of-args ((case-lambda ((a b) (+ a b))) 1))
(check-catch 'wrong-number-of-args ((case-lambda)))

(check-report)
