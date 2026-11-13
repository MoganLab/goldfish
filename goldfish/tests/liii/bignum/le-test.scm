;; <=
;; 判断实数是否单调不减。
;;
;; 语法
;; ----
;; (<= x1 x2 ...)
;;
;; 参数
;; ----
;; x1, x2 : real? 实数。
;;
;; 返回值
;; ----
;; boolean? 单调不减返回 #t，否则返回 #f。

(import (liii check) (liii bignum))

(check-set-mode! 'report-failed)

(check (<= (expt 10 30) (expt 10 30) (+ (expt 10 30) 1)) => #t)

(check-report)
