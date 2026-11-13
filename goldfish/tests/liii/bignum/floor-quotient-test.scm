;; floor-quotient
;; 计算向下取整除法的商（支持大整数与大有理数）。
;;
;; 语法
;; ----
;; (floor-quotient n1 n2)
;;
;; 参数
;; ----
;; n1, n2 : real? 实数。
;;
;; 返回值
;; ----
;; integer? 商。

(import (liii check) (liii bignum))

(check-set-mode! 'report-failed)

(check (floor-quotient (- (expt 10 20)) 3)
  =>
  (- (- (quotient (expt 10 20) 3)) 1)
) ;check

(check-report)
