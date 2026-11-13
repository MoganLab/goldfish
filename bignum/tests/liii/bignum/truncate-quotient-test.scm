;; truncate-quotient
;; 计算向零截断除法的商（支持大整数与大有理数）。
;;
;; 语法
;; ----
;; (truncate-quotient n1 n2)
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

(check (truncate-quotient (- (expt 10 20)) 3) => (- (quotient (expt 10 20) 3)))

(check-report)
