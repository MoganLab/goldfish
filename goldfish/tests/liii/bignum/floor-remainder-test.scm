;; floor-remainder
;; 计算向下取整除法的余数（支持大整数与大有理数）。
;;
;; 语法
;; ----
;; (floor-remainder n1 n2)
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

(check (floor-remainder (- (expt 10 20)) 3) => 2)

(check-report)
