;; odd?
;; 判断整数是否为奇数（支持大整数）。
;;
;; 语法
;; ----
;; (odd? n)
;;
;; 参数
;; ----
;; n : integer? 整数。
;;
;; 返回值
;; ----
;; boolean? 若为奇数返回 #t，否则返回 #f。

(import (liii check) (liii bignum))

(check-set-mode! 'report-failed)

(check (odd? (+ (expt 10 30) 1)) => #t)
(check (odd? (expt 10 30)) => #f)

(check-report)
