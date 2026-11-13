;; ceiling
;; 返回不小于参数的最小整数（支持大整数）。
;;
;; 语法
;; ----
;; (ceiling x)
;;
;; 参数
;; ----
;; x : real? 实数。
;;
;; 返回值
;; ----
;; integer? 向上取整结果。

(import (liii check) (liii bignum))

(check-set-mode! 'report-failed)

(check (ceiling (expt 10 30)) => (expt 10 30))

(check-report)
