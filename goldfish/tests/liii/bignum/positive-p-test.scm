;; positive?
;; 判断实数是否严格大于 0。
;;
;; 语法
;; ----
;; (positive? x)
;;
;; 参数
;; ----
;; x : real? 实数。
;;
;; 返回值
;; ----
;; boolean? 若大于 0 返回 #t，否则返回 #f。

(import (liii check) (liii bignum))

(check-set-mode! 'report-failed)

(check (positive? (expt 10 30)) => #t)
(check (positive? (- (expt 10 30))) => #f)

(check-report)
