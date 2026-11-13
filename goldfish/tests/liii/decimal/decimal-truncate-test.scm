;; decimal-truncate
;; 向零方向舍入到整数。
;;
;; 语法
;; ----
;; (decimal-truncate n)
;;
;; 参数
;; ----
;; n : decimal?
;;   要舍入的 decimal 值。
;;
;; 返回值
;; ----
;; decimal?
;;   向零舍入后的整数值（scale=0）。

(import (liii check)
        (liii decimal)
) ;import

(check-set-mode! 'report-failed)

(define (d str)
  (string->decimal str)
) ;define

(check (decimal->string (decimal-truncate (d "3.7"))) => "3")
(check (decimal->string (decimal-truncate (d "-3.7"))) => "-3")
(check (decimal->string (decimal-truncate (d "3.0"))) => "3")
(check (decimal->string (decimal-truncate (d "5"))) => "5")

(check-report)
