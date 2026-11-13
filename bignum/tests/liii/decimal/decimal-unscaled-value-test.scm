;; decimal-unscaled-value
;; 获取 decimal 的非标度值（unscaled value）。
;;
;; 语法
;; ----
;; (decimal-unscaled-value n)
;;
;; 参数
;; ----
;; n : decimal?
;;   输入 decimal。
;;
;; 返回值
;; ----
;; bigint?
;;   非标度值，满足数值 = unscaled × 10^(-scale)。
;;
;; 错误处理
;; ----
;; type-error
;;   当参数不是 decimal 时抛出。

(import (liii check)
        (liii bigint)
        (liii decimal)
) ;import

(check-set-mode! 'report-failed)

(define (d str)
  (string->decimal str)
) ;define

(check (bigint->string (decimal-unscaled-value (d "1.23"))) => "123")
(check (bigint->string (decimal-unscaled-value (d "-0.05"))) => "-5")
(check (bigint->string (decimal-unscaled-value (decimal-set-scale (d "1.2") 3)))
       => "1200"
) ;check

(check-catch 'type-error (decimal-unscaled-value 1))

(check-report)
