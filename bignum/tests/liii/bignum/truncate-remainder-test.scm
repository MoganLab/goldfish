;; truncate-remainder
;; 计算向零截断除法的余数（支持大整数与大有理数）。
;;
;; 语法
;; ----
;; (truncate-remainder n1 n2)
;;
;; 参数
;; ----
;; n1, n2 : real? 实数。
;;
;; 返回值
;; ----
;; real? 余数。

(import (liii check) (liii bignum))

(check-set-mode! 'report-failed)

(check (truncate-remainder (- (expt 10 20)) 3) => -1)

(check-report)
