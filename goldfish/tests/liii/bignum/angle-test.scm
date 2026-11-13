;; angle
;; 计算复数或实数的辐角（支持大整数、大有理数与大复数）。
;;
;; 语法
;; ----
;; (angle z)
;;
;; 参数
;; ----
;; z : number? 数值。
;;
;; 返回值
;; ----
;; real? 辐角（弧度）。

(import (liii check) (liii bignum))

(check-set-mode! 'report-failed)

(check (angle (expt 10 20)) => 0.0)
(check (> (angle (make-rectangular 1 2)) 1.1) => #t)

(check-report)
