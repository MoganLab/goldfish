;; floor
;; 返回不大于参数的最大整数（支持大整数）。
;;
;; 语法
;; ----
;; (floor x)
;;
;; 参数
;; ----
;; x : real? 实数。
;;
;; 返回值
;; ----
;; integer? 向下取整结果。

(import (liii check) (liii bignum))

(check-set-mode! 'report-failed)

(check (floor (expt 10 30)) => (expt 10 30))
(check (floor (/ (expt 10 20) 3))
  =>
  (quotient (expt 10 20) 3)
) ;check
(check (floor (- (/ (expt 10 20) 3)))
  =>
  (- (- (quotient (expt 10 20) 3)) 1)
) ;check

(check-report)
