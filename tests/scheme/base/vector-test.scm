(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; vector
;; 从给定元素构造向量。
;;
;; 语法
;; ----
;; (vector obj ...)
;;
;; 参数
;; ----
;; obj ... : any
;; 任意数量的元素。
;;
;; 返回值
;; ------
;; vector?
;; 由给定元素组成的新向量。
;;
;; 说明
;; ----
;; 1. 无参数时返回空向量
;; 2. 参数顺序决定结果向量中的元素顺序
(check (vector) => #())
(check (vector 'a) => #(a))
(check (vector 1 2 3) => #(1 2 3))
(check (vector "a" 'b 3) => #("a" b 3))
(check (vector #(#\a) #(#\b)) => #(#(#\a) #(#\b)))
(check (vector-length (vector 1 2 3 4)) => 4)
(check (vector-ref (vector 'x 'y) 0) => 'x)

(check-report)
