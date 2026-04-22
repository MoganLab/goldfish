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
(check (vector->list #(1 2 3))
  =>
  '(1 2 3)
) ;check
(check (vector->list #("x" "y"))
  =>
  '("x" "y")
) ;check
(check (length (vector->list #(a b c d)))
  =>
  4
) ;check
(check (car (vector->list #(1 2 3)))
  =>
  1
) ;check
(check-catch 'wrong-number-of-args
  (vector->list)
) ;check-catch
(check-catch 'wrong-type-arg
  (vector->list '())
) ;check-catch
(check-catch 'wrong-type-arg
  (vector->list "abc")
) ;check-catch

(check-report)
