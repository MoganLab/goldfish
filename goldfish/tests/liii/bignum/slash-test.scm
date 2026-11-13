;; /
;; 计算数值之商或倒数（支持大整数整除与约分）。
;;
;; 语法
;; ----
;; (/ z1 z2 ...)
;;
;; 参数
;; ----
;; z1, z2 : number? 数值。
;;
;; 返回值
;; ----
;; number? 商或倒数。

(import (liii check) (liii bignum))

(check-set-mode! 'report-failed)

(check (/ (+ 9223372036854775807 1) 2) => 4611686018427387904)
(check (/ (* 3 (expt 10 20)) (* 2 (expt 10 20))) => 3/2)
(check (/ (expt 10 20) 1/2) => (* 2 (expt 10 20)))
(check (exact? (/ (expt 10 20) 3)) => #t)
(check (* (/ (expt 10 20) 3) 3) => (expt 10 20))
(check (number->string (/ (expt 10 20) 3)) => "100000000000000000000/3")

(check-report)
