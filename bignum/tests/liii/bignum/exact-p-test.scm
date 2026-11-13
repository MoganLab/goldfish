;; exact?
;; 判断数值是否为精确数（大整数恒为精确数）。
;;
;; 语法
;; ----
;; (exact? z)
;;
;; 参数
;; ----
;; z : number? 数值。
;;
;; 返回值
;; ----
;; boolean? 若为精确数返回 #t，否则返回 #f。

(import (liii check) (liii bignum))

(check-set-mode! 'report-failed)

(check (exact? (expt 10 30)) => #t)
(check (exact? 1.5) => #f)

(check-report)
