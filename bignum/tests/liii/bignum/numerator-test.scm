;; numerator
;; 返回有理数的分子（大整数返回自身）。
;;
;; 语法
;; ----
;; (numerator q)
;;
;; 参数
;; ----
;; q : rational? 有理数。
;;
;; 返回值
;; ----
;; integer? 分子。

(import (liii check) (liii bignum))

(check-set-mode! 'report-failed)

(check (numerator (expt 10 30)) => (expt 10 30))

(check-report)
