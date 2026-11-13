;; lcm
;; 计算多个整数的最小公倍数（支持大整数）。
;;
;; 语法
;; ----
;; (lcm n ...)
;;
;; 参数
;; ----
;; n : integer? 整数。
;;
;; 返回值
;; ----
;; integer? 最小公倍数。

(import (liii check) (liii bignum))

(check-set-mode! 'report-failed)

(check (lcm (expt 2 70) (* 3 (expt 2 70))) => (* 3 (expt 2 70)))

(check-report)
