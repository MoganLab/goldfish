;; exact->inexact
;; 将精确数值转换为非精确数值。
;;
;; 语法
;; ----
;; (exact->inexact z)
;;
;; 参数
;; ----
;; z : number? 数值。
;;
;; 返回值
;; ----
;; number? 对应的非精确数值。

(import (liii check) (liii bignum))

(check-set-mode! 'report-failed)

(check (inexact? (exact->inexact (expt 10 30))) => #t)

(check-report)
