;; min
;; 返回实数序列中的最小值。
;;
;; 语法
;; ----
;; (min x1 x2 ...)
;;
;; 参数
;; ----
;; x1, x2 : real? 实数。
;;
;; 返回值
;; ----
;; real? 最小值。

(import (liii check) (liii bignum))

(check-set-mode! 'report-failed)

(check (min (- (expt 10 30)) 0 (expt 10 30)) => (- (expt 10 30)))

(check-report)
