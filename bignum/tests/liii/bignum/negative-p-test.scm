;; negative?
;; 判断实数是否严格小于 0。
;;
;; 语法
;; ----
;; (negative? x)
;;
;; 参数
;; ----
;; x : real? 实数。
;;
;; 返回值
;; ----
;; boolean? 若小于 0 返回 #t，否则返回 #f。

(import (liii check) (liii bignum))

(check-set-mode! 'report-failed)

(check (negative? (- (expt 10 30))) => #t)
(check (negative? (expt 10 30)) => #f)

(check-report)
