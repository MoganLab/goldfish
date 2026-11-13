;; remainder
;; 计算整数除法的余数（支持大整数）。
;;
;; 语法
;; ----
;; (remainder n1 n2)
;;
;; 参数
;; ----
;; n1, n2 : integer? 整数。
;;
;; 返回值
;; ----
;; integer? 余数。

(import (liii check) (liii bignum))

(check-set-mode! 'report-failed)

(check (remainder (+ (expt 10 30) 7) 10) => 7)

(check-report)
