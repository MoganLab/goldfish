;; inexact
;; 将数值转换为非精确表示。
;;
;; 语法
;; ----
;; (inexact z)
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

(check (inexact? (inexact (expt 10 30))) => #t)
(check (inexact 3) => 3.0)

(check-report)
