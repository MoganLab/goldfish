;; decimal-floor
;; 向负无穷方向舍入到整数。
;;
;; 语法
;; ----
;; (decimal-floor n)
;;
;; 参数
;; ----
;; n : decimal?
;;   要舍入的 decimal 值。
;;
;; 返回值
;; ----
;; decimal?
;;   向负无穷舍入后的整数值（scale=0）。

(import (liii check)
        (liii decimal)
) ;import

(check-set-mode! 'report-failed)

(define (d str)
  (string->decimal str)
) ;define

(check (decimal->string (decimal-floor (d "3.7"))) => "3")
(check (decimal->string (decimal-floor (d "-3.7"))) => "-4")
(check (decimal->string (decimal-floor (d "3.0"))) => "3")
(check (decimal->string (decimal-floor (d "5"))) => "5")

(check-report)
