;; infinite?
;; 判断数值是否为无穷大（支持大整数、大有理数与大复数）。
;;
;; 语法
;; ----
;; (infinite? z)
;;
;; 参数
;; ----
;; z : number? 数值。
;;
;; 返回值
;; ----
;; boolean? 无穷大返回 #t，否则返回 #f。

(import (liii check) (liii bignum))

(check-set-mode! 'report-failed)

(check (infinite? (expt 10 20)) => #f)
(check (infinite? +inf.0) => #t)

(check-report)
