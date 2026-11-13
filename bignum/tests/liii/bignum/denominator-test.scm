;; denominator
;; 返回有理数的分母（大整数返回 1）。
;;
;; 语法
;; ----
;; (denominator q)
;;
;; 参数
;; ----
;; q : rational? 有理数。
;;
;; 返回值
;; ----
;; integer? 分母。

(import (liii check) (liii bignum))

(check-set-mode! 'report-failed)

(check (denominator (expt 10 30)) => 1)

(check-report)
