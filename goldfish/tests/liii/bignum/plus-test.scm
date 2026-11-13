;; +
;; 计算多个数值之和（溢出自动提升为大整数）。
;;
;; 语法
;; ----
;; (+ z ...)
;;
;; 参数
;; ----
;; z : number? 数值。
;;
;; 返回值
;; ----
;; number? 和。

(import (liii check) (liii bignum))

(check-set-mode! 'report-failed)

(check (number->string (+ 9223372036854775807 1)) => "9223372036854775808")
(check (+) => 0)

(check-report)
