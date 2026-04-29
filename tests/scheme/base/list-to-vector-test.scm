(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; list->vector
;; 将列表转换为向量。
;;
;; 语法
;; ----
;; (list->vector list)
;;
;; 参数
;; ----
;; list : list?
;; 待转换的列表。
;;
;; 返回值
;; ------
;; vector?
;; 由列表元素按顺序组成的新向量。
;;
;; 说明
;; ----
;; 1. 空列表返回空向量
;; 2. 返回新向量，不修改原列表
(check (list->vector '()) => #())
(check (list->vector '(a)) => #(a))
(check (list->vector '(1 2 3)) => #(1 2 3))
(check (list->vector '("x" "y")) => #("x" "y"))
(check (vector-length (list->vector '(a b c d))) => 4)
(check (vector-ref (list->vector '(1 2 3)) 0) => 1)
(check-catch 'wrong-number-of-args (list->vector))
(check-catch 'wrong-type-arg (list->vector 'a))
(check-catch 'wrong-type-arg (list->vector #(1 2)))

(check-report)
