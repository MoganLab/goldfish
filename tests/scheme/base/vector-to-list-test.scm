(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; vector->list
;; 将向量转换为列表。
;;
;; 语法
;; ----
;; (vector->list vector)
;;
;; 参数
;; ----
;; vector : vector?
;; 待转换的向量。
;;
;; 返回值
;; ------
;; list?
;; 由向量元素按顺序组成的列表。
;;
;; 说明
;; ----
;; 1. 空向量返回空列表
;; 2. 返回新列表，不修改原向量
(check (vector->list #()) => '())
(check (vector->list #(a)) => '(a))
(check (vector->list #(1 2 3)) => '(1 2 3))
(check (vector->list #("x" "y")) => '("x" "y"))
(check (length (vector->list #(a b c d))) => 4)
(check (car (vector->list #(1 2 3))) => 1)

;; 带 start 和 end 参数
(check (vector->list #(0 1 2 3) 2) => '(2 3))
(check (vector->list #(0 1 2 3) 1 3) => '(1 2))
(check (vector->list #(0 1 2 3) 2 2) => '())
(check (vector->list #() 0 0) => '())

;; byte-vector 支持
(check (vector->list #u8(10 20 30)) => '(10 20 30))
(check (vector->list #u8(10 20 30) 1 3) => '(20 30))

(check-catch 'wrong-number-of-args (vector->list))
(check-catch 'wrong-number-of-args (vector->list #(1 2) 0 1 2))
(check-catch 'type-error (vector->list '()))
(check-catch 'type-error (vector->list "abc"))
(check-catch 'type-error (vector->list #(1 2 3) "0"))
(check-catch 'type-error (vector->list #(1 2 3) 0 "1"))
(check-catch 'out-of-range (vector->list #(1 2 3) -1))
(check-catch 'out-of-range (vector->list #(1 2 3) 0 4))
(check-catch 'out-of-range (vector->list #(1 2 3) 2 1))

(check-report)
