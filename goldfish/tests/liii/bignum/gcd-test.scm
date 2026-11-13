;; gcd
;; 计算多个整数的最大公约数（支持大整数）。
;;
;; 语法
;; ----
;; (gcd n ...)
;;
;; 参数
;; ----
;; n : integer? 整数。
;;
;; 返回值
;; ----
;; integer? 最大公约数。

(import (liii check) (liii bignum))

(check-set-mode! 'report-failed)

(check (gcd (expt 2 80) (* 3 (expt 2 70))) => (expt 2 70))

(check-report)
