(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; vector-length
;; 返回向量的元素个数。
;;
;; 语法
;; ----
;; (vector-length vector)
;;
;; 参数
;; ----
;; vector : vector?
;; 待查询的向量。
;;
;; 返回值
;; ------
;; exact-nonnegative-integer?
;; 向量的长度。
;;
;; 说明
;; ----
;; 1. 空向量的长度为 0
;; 2. 不修改原向量
(check (vector-length #()) => 0)
(check (vector-length #(a)) => 1)
(check (vector-length #(1 2 3)) => 3)
(check (vector-length (make-vector 10))
  =>
  10
) ;check
(check (vector-length (vector 'a 'b 'c 'd))
  =>
  4
) ;check
(check-catch 'wrong-number-of-args
  (vector-length)
) ;check-catch
(check-catch 'wrong-number-of-args
  (vector-length #(1) #(2))
) ;check-catch
(check-catch 'wrong-type-arg
  (vector-length '())
) ;check-catch
(check-catch 'wrong-type-arg
  (vector-length "abc")
) ;check-catch

(check-report)
