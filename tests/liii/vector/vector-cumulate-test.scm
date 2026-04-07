(import (liii check)
        (liii vector)
) ;import

(check-set-mode! 'report-failed)

;; vector-cumulate
;; 返回向量的累积计算结果。
;;
;; 语法
;; ----
;; (vector-cumulate proc knil vec)
;;
;; 参数
;; ----
;; proc : procedure?
;; 用于把当前元素和累积值组合成新累积值的函数。
;;
;; knil : any?
;; 初始累积值。
;;
;; vec : vector?
;; 要处理的向量。
;;
;; 返回值
;; ----
;; vector
;; 一个新向量，其中每个位置保存到该位置为止的累积结果。
;;
;; 注意
;; ----
;; 返回值是逐步累计后的所有中间结果，而不只是最终值。
;;
;; 示例
;; ----
;; (vector-cumulate + 0 #(1 2 3 4)) => #(1 3 6 10)
;;
;; 错误处理
;; ----
;; type-error 当vec不是向量时
;; wrong-type-arg 当proc无法应用到对应参数时

(check (vector-cumulate + 0 '#(1 2 3 4)) => #(1 3 6 10))
(check (vector-cumulate - 0 '#(1 2 3 4)) => #(-1 -3 -6 -10))
(check (vector-cumulate * 1 '#(-1 -2 -3 -4)) => #(-1 2 -6 24))
(check-catch 'type-error (vector-cumulate + 0 'a))
(check (vector-cumulate + 0 '#()) => #())
(check (vector-cumulate (lambda (x y) 'a) 0 '#(1 2 3)) => #(a a a))
(check-catch 'wrong-number-of-args (vector-cumulate (lambda (x) 'a) 0 '#(1 2 3)))
(check-catch 'wrong-type-arg (vector-cumulate + '(1) '#(1 2 3)))
(check (vector-cumulate (lambda (x y) (+ x 2)) 0 '#('a 'b 'c)) => #(2 4 6))

(check-report)
