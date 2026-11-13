;; decimal-subtract
;; 计算两个 decimal 的差。
;;
;; 语法
;; ----
;; (decimal-subtract a b)
;;
;; 参数
;; ----
;; a : decimal?
;;   被减数。
;; b : decimal?
;;   减数。
;;
;; 返回值
;; ----
;; decimal?
;;   a 与 b 的差，scale 取两操作数中较大者。
;;
;; 错误处理
;; ----
;; type-error
;;   当任一参数不是 decimal 时抛出。

(import (liii check)
        (liii decimal)
) ;import

(check-set-mode! 'report-failed)

(define (d str)
  (string->decimal str)
) ;define

(check (decimal->string (decimal-subtract (d "5.5") (d "0.75"))) => "4.75")
(check (decimal->string (decimal-subtract (d "1.00") (d "0.99"))) => "0.01")

(check-catch 'type-error (decimal-subtract (d "1") 1))

(check-report)
