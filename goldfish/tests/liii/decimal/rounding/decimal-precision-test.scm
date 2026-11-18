;; decimal-precision
;; 返回 decimal 的精度（unscaled-value 的数字个数）。
;;
;; 语法
;; ----
;; (decimal-precision n)
;;
;; 参数
;; ----
;; n : decimal?
;;   输入 decimal。
;;
;; 返回值
;; ----
;; integer?
;;   unscaled-value 的十进制数字个数；零返回 1。
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

;; precision 基于 unscaled-value 的数字个数
(check (decimal-precision (d "0")) => 1)
(check (decimal-precision (d "1.23")) => 3)
(check (decimal-precision (d "1200")) => 4)

;; "1.200" 的 unscaled-value 是 1200，所以 precision 是 4（不是 2）
(check (decimal-precision (d "1.200")) => 4)

(check (decimal-precision (d "-0.05")) => 1)
(check (decimal-precision (decimal-set-scale (d "1.2") 3)) => 4)

(check-catch 'type-error (decimal-precision 1))

(check-report)
