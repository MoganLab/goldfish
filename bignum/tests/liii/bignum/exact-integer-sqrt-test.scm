;; exact-integer-sqrt
;; 计算非负整数的精确整数平方根及余数（支持大整数）。
;;
;; 语法
;; ----
;; (exact-integer-sqrt k)
;;
;; 参数
;; ----
;; k : exact-integer? 非负精确整数。
;;
;; 返回值
;; ----
;; values 两个值 s 和 r，满足 k = s^2 + r 且 k < (s+1)^2。

(import (liii check) (liii bignum))

(check-set-mode! 'report-failed)

(check
  (call-with-values
    (lambda () (exact-integer-sqrt (+ (expt 10 20) 5)))
    list
  ) ;call-with-values
  =>
  (list (expt 10 10) 5)
) ;check

(check-report)
