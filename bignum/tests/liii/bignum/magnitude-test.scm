;; magnitude
;; 计算复数或实数的模（支持大整数、大有理数与大复数）。
;;
;; 语法
;; ----
;; (magnitude z)
;;
;; 参数
;; ----
;; z : number? 数值。
;;
;; 返回值
;; ----
;; real? 模长。

(import (liii check) (liii bignum))

(check-set-mode! 'report-failed)

(check (magnitude 3+4i) => 5)
(check (magnitude -100000000000000000000) => 100000000000000000000)

(check-report)
