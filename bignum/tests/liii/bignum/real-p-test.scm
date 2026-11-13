;; real?
;; 判断对象是否为实数（含大整数）。
;;
;; 语法
;; ----
;; (real? obj)
;;
;; 参数
;; ----
;; obj : any? 任意对象。
;;
;; 返回值
;; ----
;; boolean? 若为实数返回 #t，否则返回 #f。

(import (liii check) (liii bignum))

(check-set-mode! 'report-failed)

(check (real? (expt 10 30)) => #t)
(check (real? 1.0+2.0i) => #f)

(check-report)
