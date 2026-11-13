;; truncate
;; 向零截断取整（支持大整数）。
;;
;; 语法
;; ----
;; (truncate x)
;;
;; 参数
;; ----
;; x : real? 实数。
;;
;; 返回值
;; ----
;; integer? 截断结果。

(import (liii check) (liii bignum))

(check-set-mode! 'report-failed)

(check (truncate (- (expt 10 30))) => (- (expt 10 30)))

(check-report)
