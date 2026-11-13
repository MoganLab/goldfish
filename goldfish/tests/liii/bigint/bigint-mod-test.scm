;; bigint-mod
;; 返回与 a 模 m 同余的非负结果。
;;
;; 语法
;; ----
;; (bigint-mod a m)
;;
;; 参数
;; ----
;; a : bigint?
;;   待取模的值。
;; m : bigint?
;;   正模数。
;;
;; 返回值
;; ----
;; bigint?
;;   落在 [0, m-1] 范围内的结果。
;;
;; 错误处理
;; ----
;; type-error / value-error
;;   参数类型错误时抛出 type-error；模数非正时抛出 value-error。

(import (liii check)
        (liii bigint)
) ;import

(check-set-mode! 'report-failed)
(define (b str)
  (string->bigint str)
) ;define

(check (bigint->string (bigint-mod (b "-17") (b "5"))) => "3")
(check (bigint->string (bigint-mod (b "17") (b "5"))) => "2")
(check (bigint->string (bigint-mod bigint-zero (b "7"))) => "0")

(check-catch 'type-error (bigint-mod 17 (b "5")))
(check-catch 'value-error (bigint-mod (b "17") bigint-zero))
(check-catch 'value-error (bigint-mod (b "17") (b "-5")))

(check-report)
