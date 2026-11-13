;; number->string
;; 将数值（含大整数）格式化为字符串。
;;
;; 语法
;; ----
;; (number->string z [radix])
;;
;; 参数
;; ----
;; z : number? 数值。
;; radix
;; :
;; integer?
;; 可选进制，默认
;; 10。
;;
;; 返回值
;; ----
;; string? 数值的字符串表示。

(import (liii check) (liii bignum))

(check-set-mode! 'report-failed)

(check (number->string (expt 2 64) 16) => "10000000000000000")
(check (number->string (expt 10 19)) => "10000000000000000000")

(check-report)
