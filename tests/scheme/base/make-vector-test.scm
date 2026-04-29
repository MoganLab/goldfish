(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; make-vector
;; 创建指定长度的新向量，可选填充值。
;;
;; 语法
;; ----
;; (make-vector k [fill])
;;
;; 参数
;; ----
;; k : exact-nonnegative-integer?
;; 向量的长度，必须是非负整数。
;; fill : any
;; 可选，用于填充向量的值，默认为未指定。
;;
;; 返回值
;; ------
;; vector?
;; 长度为 k 的新向量。
;;
;; 说明
;; ----
;; 1. k 必须是非负整数
;; 2. 不提供 fill 时，各元素的初始值由实现决定
(check (make-vector 0) => #())
(check (vector-length (make-vector 3)) => 3)
(check (make-vector 3 #\a) => #(#\a #\a #\a))
(check (make-vector 2 'x) => #(x x))
(check (make-vector 1 42) => #(42))
(check-catch 'wrong-number-of-args (make-vector))
(check-catch 'wrong-type-arg (make-vector -1))
(check-catch 'wrong-type-arg (make-vector 'a))
(check-catch 'wrong-type-arg (make-vector 1.5))

(check-report)
