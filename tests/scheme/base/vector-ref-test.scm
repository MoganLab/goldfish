(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; vector-ref
;; 返回向量指定位置的元素。
;;
;; 语法
;; ----
;; (vector-ref vector k)
;;
;; 参数
;; ----
;; vector : vector?
;; 待访问的向量。
;; k : exact-nonnegative-integer?
;; 元素的索引，从 0 开始。
;;
;; 返回值
;; ------
;; any
;; 向量中索引为 k 的元素。
;;
;; 说明
;; ----
;; 1. k 必须满足 0 <= k < (vector-length vector)
;; 2. 索引从 0 开始
(check (vector-ref #(a b c) 0) => 'a)
(check (vector-ref #(a b c) 1) => 'b)
(check (vector-ref #(a b c) 2) => 'c)
(check (vector-ref #(1) 0) => 1)
(check (vector-ref #("x" "y") 1) => "y")
(check-catch 'wrong-number-of-args (vector-ref #(1)))
(check-catch 'wrong-type-arg (vector-ref '() 0))
(check-catch 'wrong-type-arg (vector-ref #(1) 'a))
(check-catch 'out-of-range (vector-ref #() 0))
(check-catch 'out-of-range (vector-ref #(a) 1))
(check-catch 'out-of-range (vector-ref #(a b) -1))

(check-report)
