;; decimal-divide-and-remainder
;; 同时返回两个 decimal 相除的整数商和余数。
;;
;; 语法
;; ----
;; (decimal-divide-and-remainder a b)
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
;; two values: quotient, remainder
;;   整数商（scale=0）与余数（符号与被除数相同）。
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

(call-with-values
    (lambda ()
      (decimal-divide-and-remainder (d "5.9") (d "2"))
    ) ;lambda
  (lambda (quotient remainder)
    (check (decimal->string quotient) => "2")
    (check (decimal->string remainder) => "1.9")
  ) ;lambda
) ;call-with-values

(call-with-values
    (lambda ()
      (decimal-divide-and-remainder (d "-5.9") (d "2"))
    ) ;lambda
  (lambda (quotient remainder)
    (check (decimal->string quotient) => "-2")
    (check (decimal->string remainder) => "-1.9")
  ) ;lambda
) ;call-with-values

(check-catch 'value-error (decimal-divide-and-remainder (d "1") (d "0")))
(check-catch 'type-error (decimal-divide-and-remainder 1 (d "2")))

(check-report)
