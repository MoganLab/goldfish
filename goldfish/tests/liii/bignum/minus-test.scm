;; -
;; 计算数值之差或取相反数（溢出自动提升并支持降级）。
;;
;; 语法
;; ----
;; (- z1 z2 ...)
;;
;; 参数
;; ----
;; z1, z2 : number? 数值。
;;
;; 返回值
;; ----
;; number? 差或相反数。

(import (liii check) (liii bignum))

(check-set-mode! 'report-failed)

(check (- (+ 9223372036854775807 1) 1) => 9223372036854775807)
(check (number->string (- -9223372036854775808)) => "9223372036854775808")

(check-report)
