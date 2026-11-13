;; bigint-rootn-and-remainder
;; 同时返回整数 n 次根及余数。
;;
;; 语法
;; ----
;; (bigint-rootn-and-remainder n degree)
;;
;; 参数
;; ----
;; n : bigint?
;;   输入值。
;; degree : integer?
;;   正整数根指数。
;;
;; 返回值
;; ----
;; values
;;   返回两个值：整数 n 次根与余数 `n - root^degree`。
;;
;; 错误处理
;; ----
;; type-error / value-error
;;   参数类型不对时抛出 type-error；根指数非正或负数偶次根时抛出 value-error。

(import (liii check)
        (liii bigint)
) ;import

(check-set-mode! 'report-failed)
(define (b str)
  (string->bigint str)
) ;define

(call-with-values
    (lambda ()
      (bigint-rootn-and-remainder (b "28") 3)
    ) ;lambda
  (lambda (root remainder)
    (check (bigint->string root) => "3")
    (check (bigint->string remainder) => "1")
  ) ;lambda
) ;call-with-values

(call-with-values
    (lambda ()
      (bigint-rootn-and-remainder (b "-30") 3)
    ) ;lambda
  (lambda (root remainder)
    (check (bigint->string root) => "-3")
    (check (bigint->string remainder) => "-3")
  ) ;lambda
) ;call-with-values

(call-with-values
    (lambda ()
      (bigint-rootn-and-remainder (b "81") 4)
    ) ;lambda
  (lambda (root remainder)
    (check (bigint->string root) => "3")
    (check (bigint->string remainder) => "0")
  ) ;lambda
) ;call-with-values

(check-catch 'value-error
             (call-with-values
                 (lambda ()
                   (bigint-rootn-and-remainder (b "-16") 2)
                 ) ;lambda
               (lambda (root remainder) root)
             ) ;call-with-values
) ;check-catch

(check-report)
