(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; let
;; 创建局部变量绑定。
;;
;; 语法
;; ----
;; (let ((var init) ...) body ...)
;; (let name ((var init) ...) body ...)
;;
;; 参数
;; ----
;; var : symbol
;; 变量名。
;;
;; init : any
;; 初始值表达式。
;;
;; body ... : any
;; 表达式体。
;;
;; name : symbol
;; 可选的循环名，用于创建命名 let。
;;
;; 返回值
;; -----
;; any
;; 返回最后一个 body 表达式的结果。
;; 基础测试 - 单变量绑定
(check (let ((x 1)) x) => 1)
;; 多变量绑定
(check (let ((x 1) (y 2)) (+ x y)) => 3)
;; 嵌套 let - 内层变量遮蔽外层变量
(check (let ((x 1))
         (let ((x 2))
           x
         ) ;let
       ) ;let
  =>
  2
) ;check
;; let 中使用条件表达式
(check (let ((x 1))
         (if (> x 0) x -x)
       ) ;let
  =>
  1
) ;check
;; 命名 let - 实现循环
(check (let loop
         ((n 5) (acc 0))
         (if (zero? n)
           acc
           (loop (- n 1) (+ acc n))
         ) ;if
       ) ;let
  =>
  15
) ;check
;; 命名 let - 实现递归
(check (let factorial
         ((n 5))
         (if (= n 1) 1 (* n (factorial (- n 1))))
       ) ;let
  =>
  120
) ;check
;; 多参数命名 let
(check (let sum
         ((a 3) (b 4))
         (+ a b)
       ) ;let
  =>
  7
) ;check
;; 嵌套命名 let
(check (let outer
         ((x 2))
         (let inner
           ((y 3))
           (+ x y)
         ) ;let
       ) ;let
  =>
  5
) ;check
(check-report)