;; bigint-divide-and-remainder
;; 同时返回截断除法的商和余数。
;;
;; 语法
;; ----
;; (bigint-divide-and-remainder a b)
;;
;; 参数
;; ----
;; a : bigint?
;;   被除数。
;; b : bigint?
;;   除数。
;;
;; 返回值
;; ----
;; values
;;   返回两个值：商与余数。
;;
;; 错误处理
;; ----
;; type-error / value-error
;;   当参数不是 bigint 时抛出 type-error；除数为零时抛出 value-error。

(import (liii check)
        (liii bigint)
) ;import

(check-set-mode! 'report-failed)
(define (b str)
  (string->bigint str)
) ;define

(call-with-values
    (lambda ()
      (bigint-divide-and-remainder (b "100") (b "3"))
    ) ;lambda
  (lambda (q r)
    (check (bigint->string q) => "33")
    (check (bigint->string r) => "1")
  ) ;lambda
) ;call-with-values

(call-with-values
    (lambda ()
      (bigint-divide-and-remainder (b "-100") (b "3"))
    ) ;lambda
  (lambda (q r)
    (check (bigint->string q) => "-33")
    (check (bigint->string r) => "-1")
  ) ;lambda
) ;call-with-values

(check-catch 'value-error
             (call-with-values
                 (lambda ()
                   (bigint-divide-and-remainder (b "1") bigint-zero)
                 ) ;lambda
               (lambda (q r) q)
             ) ;call-with-values
) ;check-catch

(check-report)
