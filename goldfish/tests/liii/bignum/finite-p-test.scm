;; finite?
;; 判断数值是否为有限数（支持大整数、大有理数与大复数）。
;;
;; 语法
;; ----
;; (finite? z)
;;
;; 参数
;; ----
;; z : number? 数值。
;;
;; 返回值
;; ----
;; boolean? 有限返回 #t，否则返回 #f。

(import (liii check) (liii bignum))

(check-set-mode! 'report-failed)

(check (finite? (expt 10 20)) => #t)
(check (finite? (/ (expt 10 20) 3)) => #t)
(check (finite? (make-rectangular 1 2)) => #t)
(check (finite? +inf.0) => #f)

(check-report)
