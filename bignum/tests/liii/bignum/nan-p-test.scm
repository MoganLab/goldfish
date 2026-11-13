;; nan?
;; 判断数值是否为 NaN（支持大整数、大有理数与大复数）。
;;
;; 语法
;; ----
;; (nan? z)
;;
;; 参数
;; ----
;; z : number? 数值。
;;
;; 返回值
;; ----
;; boolean? 为 NaN 返回 #t，否则返回 #f。

(import (liii check) (liii bignum))

(check-set-mode! 'report-failed)

(check (nan? (expt 10 20)) => #f)
(check (nan? +nan.0) => #t)

(check-report)
