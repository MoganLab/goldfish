(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; define
;; 定义变量或过程。
;;
;; 语法
;; ----
;; (define variable expression)
;; (define (variable formals) body)
;;
;; 参数
;; ----
;; variable : symbol?
;; 定义的变量或过程名。
;; expression : 任意类型
;; 变量初始值。
;; formals : 参数列表
;; 过程参数。
;; body : 表达式序列
;; 过程体。
;;
;; 返回值
;; ------
;; 未指定
;;
;; 说明
;; ----
;; 1. 第一种形式定义变量
;; 2. 第二种形式是 lambda 的语法糖
;; 3. 在顶层或函数体内使用
(define test-var 42)
(check test-var => 42)
(define (square x)
  (* x x)
) ;define
(check (square 5) => 25)
(define (add a b)
  (+ a b)
) ;define
(check (add 2 3) => 5)

(check-report)
