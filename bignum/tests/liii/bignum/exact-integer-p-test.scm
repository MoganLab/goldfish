;; exact-integer?
;; 判断对象是否为精确整数（含大整数）。
;;
;; 语法
;; ----
;; (exact-integer? obj)
;;
;; 参数
;; ----
;; obj : any? 任意对象。
;;
;; 返回值
;; ----
;; boolean? 若为精确整数返回 #t，否则返回 #f。

(import (liii check) (liii bignum))

(check-set-mode! 'report-failed)

(check (exact-integer? (expt 10 30)) => #t)
(check (exact-integer? 1.0) => #f)

(check-report)
