;; decimal-remainder
;; 计算两个 decimal 相除的余数（符号与被除数相同）。
;;
;; 语法
;; ----
;; (decimal-remainder a b)
;;
;; 参数
;; ----
;; a : decimal?
;;   被除数。
;; b : decimal?
;;   除数。
;;
;; 返回值
;; ----
;; decimal?
;;   余数，符号与被除数相同。
;;
;; 错误处理
;; ----
;; value-error
;;   当除数为零时抛出。
;; type-error
;;   当任一参数不是 decimal 时抛出。

(import (liii check)
        (liii decimal)
) ;import

(check-set-mode! 'report-failed)

(define (d str)
  (string->decimal str)
) ;define

(check (decimal->string (decimal-remainder (d "5.9") (d "2"))) => "1.9")
(check (decimal->string (decimal-remainder (d "-5.9") (d "2"))) => "-1.9")
(check (decimal->string (decimal-remainder (d "1.23") (d "0.1"))) => "0.03")

(check-catch 'value-error (decimal-remainder (d "1") (d "0")))
(check-catch 'type-error (decimal-remainder 1 (d "2")))

(check-report)
