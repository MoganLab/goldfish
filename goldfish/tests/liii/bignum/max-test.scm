;; max
;; 返回实数序列中的最大值。
;;
;; 语法
;; ----
;; (max x1 x2 ...)
;;
;; 参数
;; ----
;; x1, x2 : real? 实数。
;;
;; 返回值
;; ----
;; real? 最大值。

(import (liii check) (liii bignum))

(check-set-mode! 'report-failed)

(check (max 1 (+ 9223372036854775807 1) (expt 10 30)) => (expt 10 30))

(check-report)
