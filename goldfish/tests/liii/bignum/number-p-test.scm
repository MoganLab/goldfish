;; number?
;; 判断对象是否为数值（含大整数）。
;;
;; 语法
;; ----
;; (number? obj)
;;
;; 参数
;; ----
;; obj : any? 任意对象。
;;
;; 返回值
;; ----
;; boolean? 若为数值返回 #t，否则返回 #f。

(import (liii check) (liii bignum))

(check-set-mode! 'report-failed)

(check (number? (expt 10 30)) => #t)
(check (number? "abc") => #f)

(check-report)
