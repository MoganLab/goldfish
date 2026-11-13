;; decimal-divide-to-integral-value
;; 返回两个 decimal 相除的整数部分（向零截断）。
;;
;; 语法
;; ----
;; (decimal-divide-to-integral-value a b)
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
;;   商的整数部分（scale=0）。
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

(check (decimal->string (decimal-divide-to-integral-value (d "5.9") (d "2")))
       => "2"
) ;check
(check (decimal->string (decimal-divide-to-integral-value (d "-5.9") (d "2")))
       => "-2"
) ;check
(check (decimal->string (decimal-divide-to-integral-value (d "1.23") (d "0.1")))
       => "12"
) ;check

(check-catch 'value-error (decimal-divide-to-integral-value (d "1") (d "0")))
(check-catch 'type-error (decimal-divide-to-integral-value 1 (d "2")))

(check-report)
