;; decimal-exact-integer?
;; 判断 decimal 是否为精确整数（即小数部分为零）。
;;
;; 语法
;; ----
;; (decimal-exact-integer? n)
;;
;; 参数
;; ----
;; n : decimal?
;;   要判断的 decimal 值。
;;
;; 返回值
;; ----
;; boolean?
;;   如果 n 的小数部分为零则返回 #t，否则返回 #f。
;;
;; 说明
;; ----
;; 即使 scale 不为 0，只要小数部分实际为零（如 1.000），也返回 #t。
;; 该函数通过检查 unscaled-value 是否能被 10^scale 整除来判断。
;;
;; 错误处理
;; ----
;; type-error
;;   当输入不是 decimal 时抛出。

(import (liii check)
        (liii bigint)
        (liii decimal)
) ;import

(check-set-mode! 'report-failed)
(define (d str)
  (string->decimal str)
) ;define

(check-true (decimal-exact-integer? (d "1")))
(check-true (decimal-exact-integer? (decimal-set-scale (d "1") 3)))
(check-true (decimal-exact-integer? (decimal-set-scale (d "0") 4)))
(check-true (decimal-exact-integer? (make-decimal (string->bigint "1") -3)))
(check-true (decimal-exact-integer? (decimal-set-scale (d "100") -2)))
(check-false (decimal-exact-integer? (d "1.2")))
(check-false (decimal-exact-integer? (d "-0.5")))

(check-catch 'type-error (decimal-exact-integer? 1))

(check-report)
