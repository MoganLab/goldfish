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

(import (liii check))
(import (scheme base))

(check-set-mode! 'report-failed)

;; 基础测试 - 单变量绑定
(check (let ((x 1)) x) => 1)
;; 多变量绑定
(check (let ((x 1) (y 2)) (+ x y)) => 3)
;; 嵌套 let - 内层变量遮蔽外层变量
(check
  (let ((x 1))
    (let ((x 2))
      x
    ) ;let
  ) ;let
  =>
  2
) ;check
;; let 中使用条件表达式
(check (let ((x 1)) (if (> x 0) x -x)) => 1)
;; 命名 let - 实现循环
(check
  (let loop
    ((n 5) (acc 0))
    (if (zero? n) acc (loop (- n 1) (+ acc n)))
  ) ;let
  =>
  15
) ;check
;; 命名 let - 实现递归
(check
  (let factorial
    ((n 5))
    (if (= n 1) 1 (* n (factorial (- n 1))))
  ) ;let
  =>
  120
) ;check
;; 多参数命名 let
(check (let sum ((a 3) (b 4)) (+ a b)) => 7)
;; 嵌套命名 let
(check
  (let outer
    ((x 2))
    (let inner
      ((y 3))
      (+ x y)
    ) ;let
  ) ;let
  =>
  5
) ;check

;; issue #1337：命名 let 的循环实参中求值递归调用时，递归返回后必须恢复外层帧。
;; 分别覆盖 OP_ANY_CLOSURE_3P、4P、NP 三条求值路径。

(define (issue-1337-named-let-3 value depth)
  (let loop
    ((i 0) (dummy 'x) (seen '()))
    (if (= i 2)
      (reverse seen)
      (loop (+ i 1)
        'y
        (append
          (reverse (cons (list value depth i)
                     (if (= depth 0) (issue-1337-named-let-3 'INNER 1) '())
                   ) ;cons
          ) ;reverse
          seen
        ) ;append
      ) ;loop
    ) ;if
  ) ;let
) ;define

(define (issue-1337-named-let-4 value depth)
  (let loop
    ((i 0) (dummy1 'x) (dummy2 'y) (seen '()))
    (if (= i 2)
      (reverse seen)
      (loop (+ i 1)
        'a
        'b
        (append
          (reverse (cons (list value depth i)
                     (if (= depth 0) (issue-1337-named-let-4 'INNER 1) '())
                   ) ;cons
          ) ;reverse
          seen
        ) ;append
      ) ;loop
    ) ;if
  ) ;let
) ;define

(define (issue-1337-named-let-5 value depth)
  (let loop
    ((i 0) (dummy1 'x) (dummy2 'y) (dummy3 'z) (seen '()))
    (if (= i 2)
      (reverse seen)
      (loop (+ i 1)
        'a
        'b
        'c
        (append
          (reverse (cons (list value depth i)
                     (if (= depth 0) (issue-1337-named-let-5 'INNER 1) '())
                   ) ;cons
          ) ;reverse
          seen
        ) ;append
      ) ;loop
    ) ;if
  ) ;let
) ;define

(define issue-1337-expected
  '((OUTER 0 0) (INNER 1 0) (INNER 1 1) (OUTER 0 1) (INNER 1 0) (INNER 1 1))
) ;define

(check (issue-1337-named-let-3 'OUTER 0) => issue-1337-expected)
(check (issue-1337-named-let-4 'OUTER 0) => issue-1337-expected)
(check (issue-1337-named-let-5 'OUTER 0) => issue-1337-expected)
(check-report)
