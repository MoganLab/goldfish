;; sqrt
;; 计算平方根（完全平方大整数与大有理数返回精确结果）。
;;
;; 语法
;; ----
;; (sqrt z)
;;
;; 参数
;; ----
;; z : number? 数值。
;;
;; 返回值
;; ----
;; number? 平方根。

(import (liii check) (liii bignum))

(check-set-mode! 'report-failed)

(check (sqrt (expt 10 20)) => (expt 10 10))
(check (number->string (sqrt (/ (expt 10 20) 9))) => "10000000000/3")

(check-report)
