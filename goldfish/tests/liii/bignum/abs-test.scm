;; abs
;; 计算实数的绝对值（支持 INT64_MIN 与大整数）。
;;
;; 语法
;; ----
;; (abs x)
;;
;; 参数
;; ----
;; x : real? 实数。
;;
;; 返回值
;; ----
;; real? 绝对值。

(import (liii check) (liii bignum))

(check-set-mode! 'report-failed)

(check (number->string (abs -9223372036854775808)) => "9223372036854775808")
(check (abs (- (expt 10 30))) => (expt 10 30))

(check-report)
