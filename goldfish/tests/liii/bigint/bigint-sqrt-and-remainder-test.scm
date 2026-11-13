;; bigint-sqrt-and-remainder
;; 同时返回平方根整数部分和余数。
;;
;; 语法
;; ----
;; (bigint-sqrt-and-remainder n)
;;
;; 参数
;; ----
;; n : bigint?
;;   非负输入值。
;;
;; 返回值
;; ----
;; values
;;   返回两个值：平方根整数部分与余数。
;;
;; 错误处理
;; ----
;; type-error / value-error
;;   参数不是 bigint 时抛出 type-error；输入为负数时抛出 value-error。

(import (liii check)
        (liii bigint)
) ;import

(check-set-mode! 'report-failed)
(define (b str)
  (string->bigint str)
) ;define

(call-with-values
    (lambda ()
      (bigint-sqrt-and-remainder bigint-zero)
    ) ;lambda
  (lambda (root remainder)
    (check (bigint->string root) => "0")
    (check (bigint->string remainder) => "0")
  ) ;lambda
) ;call-with-values

(call-with-values
    (lambda ()
      (bigint-sqrt-and-remainder (b "15"))
    ) ;lambda
  (lambda (root remainder)
    (check (bigint->string root) => "3")
    (check (bigint->string remainder) => "6")
  ) ;lambda
) ;call-with-values

(call-with-values
    (lambda ()
      (bigint-sqrt-and-remainder (b "144"))
    ) ;lambda
  (lambda (root remainder)
    (check (bigint->string root) => "12")
    (check (bigint->string remainder) => "0")
  ) ;lambda
) ;call-with-values

(check-catch 'type-error
             (call-with-values
                 (lambda ()
                   (bigint-sqrt-and-remainder 9)
                 ) ;lambda
               (lambda (root remainder) root)
             ) ;call-with-values
) ;check-catch

(check-catch 'value-error
             (call-with-values
                 (lambda ()
                   (bigint-sqrt-and-remainder (b "-1"))
                 ) ;lambda
               (lambda (root remainder) root)
             ) ;call-with-values
) ;check-catch

(check-report)
