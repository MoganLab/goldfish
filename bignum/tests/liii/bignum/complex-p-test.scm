;; complex?
;; 判断对象是否为复数（含大整数）。
;;
;; 语法
;; ----
;; (complex? obj)
;;
;; 参数
;; ----
;; obj : any? 任意对象。
;;
;; 返回值
;; ----
;; boolean? 若为复数返回 #t，否则返回 #f。

(import (liii check) (liii bignum))

(check-set-mode! 'report-failed)

(check (complex? (expt 10 30)) => #t)
(check (complex? 1.0+2.0i) => #t)

(check-report)
