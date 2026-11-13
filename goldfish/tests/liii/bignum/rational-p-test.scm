;; rational?
;; 判断对象是否为有理数（含大整数）。
;;
;; 语法
;; ----
;; (rational? obj)
;;
;; 参数
;; ----
;; obj : any? 任意对象。
;;
;; 返回值
;; ----
;; boolean? 若为有理数返回 #t，否则返回 #f。

(import (liii check) (liii bignum))

(check-set-mode! 'report-failed)

(check (rational? (expt 10 30)) => #t)
(check (rational? 3/4) => #t)

(check-report)
