;; bignum?
;; 判断对象是否为任意精度大整数。
;;
;; 语法
;; ----
;; (bignum? obj)
;;
;; 参数
;; ----
;; obj : any? 任意对象。
;;
;; 返回值
;; ----
;; boolean? 若为大整数返回 #t，否则返回 #f。

(import (liii check) (liii bignum))

(check-set-mode! 'report-failed)

(check (bignum? (+ 9223372036854775807 1)) => #t)
(check (bignum? 42) => #f)

(check-report)
