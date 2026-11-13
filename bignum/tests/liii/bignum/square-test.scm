;; square
;; 计算数值的平方（支持大整数、大有理数与大复数）。
;;
;; 语法
;; ----
;; (square z)
;;
;; 参数
;; ----
;; z : number? 数值。
;;
;; 返回值
;; ----
;; number? 平方结果。

(import (liii check) (liii bignum))

(check-set-mode! 'report-failed)

(check (square 10000000000) => 100000000000000000000)
(check (square 1+2i) => -3+4i)

(check-report)
