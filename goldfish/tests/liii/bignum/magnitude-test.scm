;; magnitude
;; 计算复数或实数的模（支持大整数、大有理数与大复数）。
;;
;; 语法
;; ----
;; (magnitude z)
;;
;; 参数
;; ----
;; z : number? 数值。
;;
;; 返回值
;; ----
;; real? 模长。

(import (liii check) (liii bignum))

(check-set-mode! 'report-failed)

(check (magnitude (make-rectangular 3 4)) => 5)
(check (magnitude (- (expt 10 20))) => (expt 10 20))

(check-report)
