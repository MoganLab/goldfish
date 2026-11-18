;; decimal-unscaled
;; 获取 decimal 的非标度值（bigint）。
;;
;; 语法
;; ----
;; (decimal-unscaled n)
;;
;; 参数
;; ----
;; n : decimal?
;;   输入 decimal。
;;
;; 返回值
;; ----
;; bigint?
;;   decimal 的非标度值。

(import (liii check)
        (liii decimal)
        (liii decimal record)
        (liii bigint)
        (liii bigint conversion)
) ;import

(check-set-mode! 'report-failed)

(define (d str)
  (string->decimal str)
) ;define

(check (bigint->string (decimal-unscaled (d "1.23"))) => "123")
(check (bigint->string (decimal-unscaled (d "-45.6"))) => "-456")
(check (bigint->string (decimal-unscaled (d "0"))) => "0")
(check (bigint->string (decimal-unscaled (d "100"))) => "100")

(check (bigint? (decimal-unscaled (d "1.23"))) => #t)

(check-report)
