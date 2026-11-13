;; inexact->exact
;; 将非精确数值转换为精确数值。
;;
;; 语法
;; ----
;; (inexact->exact z)
;;
;; 参数
;; ----
;; z : number? 数值。
;;
;; 返回值
;; ----
;; number? 对应的精确数值。

(import (liii check) (liii bignum))

(check-set-mode! 'report-failed)

(check (inexact->exact (expt 10 30)) => (expt 10 30))
(check (inexact->exact 2.5) => 5/2)

(check-report)
