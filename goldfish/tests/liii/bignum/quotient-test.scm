;; quotient
;; 计算整数除法的截断商（支持大整数）。
;;
;; 语法
;; ----
;; (quotient n1 n2)
;;
;; 参数
;; ----
;; n1, n2 : integer? 整数。
;;
;; 返回值
;; ----
;; integer? 商。

(import (liii check) (liii bignum))

(check-set-mode! 'report-failed)

(check (quotient (expt 10 30) (expt 10 29)) => 10)

(check-report)
