;; decimal-scale
;; 获取 decimal 的标度（小数位数）。
;;
;; 语法
;; ----
;; (decimal-scale n)
;;
;; 参数
;; ----
;; n : decimal?
;;   输入 decimal。
;;
;; 返回值
;; ----
;; integer?
;;   decimal 的 scale：数值等于 unscaled * 10^(-scale)。

(import (liii check)
        (liii decimal)
        (liii decimal record)
) ;import

(check-set-mode! 'report-failed)

(define (d str)
  (string->decimal str)
) ;define

(check (decimal-scale (d "1.23")) => 2)
(check (decimal-scale (d "100")) => 0)
(check (decimal-scale (d "0.001")) => 3)
(check (decimal-scale (d "-45.60")) => 2)

(check (integer? (decimal-scale (d "1.23"))) => #t)

(check-report)
