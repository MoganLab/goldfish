;; inexact?
;; 判断数值是否为非精确数。
;;
;; 语法
;; ----
;; (inexact? z)
;;
;; 参数
;; ----
;; z : number? 数值。
;;
;; 返回值
;; ----
;; boolean? 若为非精确数返回 #t，否则返回 #f。

(import (liii check) (liii bignum))

(check-set-mode! 'report-failed)

(check (inexact? (expt 10 30)) => #f)
(check (inexact? 1.5) => #t)

(check-report)
