;; integer?
;; 判断对象是否为整数（含大整数）。
;;
;; 语法
;; ----
;; (integer? obj)
;;
;; 参数
;; ----
;; obj : any? 任意对象。
;;
;; 返回值
;; ----
;; boolean? 若为整数返回 #t，否则返回 #f。

(import (liii check) (liii bignum))

(check-set-mode! 'report-failed)

(check (integer? (expt 10 30)) => #t)
(check (integer? 1.5) => #f)

(check-report)
