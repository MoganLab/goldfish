;; even?
;; 判断整数是否为偶数（支持大整数）。
;;
;; 语法
;; ----
;; (even? n)
;;
;; 参数
;; ----
;; n : integer? 整数。
;;
;; 返回值
;; ----
;; boolean? 若为偶数返回 #t，否则返回 #f。

(import (liii check) (liii bignum))

(check-set-mode! 'report-failed)

(check (even? (expt 10 30)) => #t)
(check (even? (+ (expt 10 30) 1)) => #f)

(check-report)
