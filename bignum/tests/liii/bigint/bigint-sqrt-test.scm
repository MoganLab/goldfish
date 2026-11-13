;; bigint-sqrt
;; 返回不超过输入值平方根的最大整数。
;;
;; 语法
;; ----
;; (bigint-sqrt n)
;;
;; 参数
;; ----
;; n : bigint?
;;   非负输入值。
;;
;; 返回值
;; ----
;; bigint?
;;   平方根的整数部分。
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

(check (bigint->string (bigint-sqrt bigint-zero)) => "0")
(check (bigint->string (bigint-sqrt bigint-one)) => "1")
(check (bigint->string (bigint-sqrt (b "15"))) => "3")
(check (bigint->string (bigint-sqrt (b "144"))) => "12")
(check (bigint->string (bigint-sqrt (b "15241578750190521"))) => "123456789")

(check-catch 'type-error (bigint-sqrt 9))
(check-catch 'value-error (bigint-sqrt (b "-1")))

(check-report)
