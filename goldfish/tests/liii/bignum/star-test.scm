;; *
;; 计算多个数值之积（溢出自动提升为大整数）。
;;
;; 语法
;; ----
;; (* z ...)
;;
;; 参数
;; ----
;; z : number? 数值。
;;
;; 返回值
;; ----
;; number? 积。

(import (liii check) (liii bignum))

(check-set-mode! 'report-failed)

(check (number->string (* 10000000000 10000000000)) => "100000000000000000000")
(check (*) => 1)

(check-report)
