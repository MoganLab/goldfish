;; bigint-mod-expt
;; 返回 base^exponent mod m。
;;
;; 语法
;; ----
;; (bigint-mod-expt base exponent m)
;;
;; 参数
;; ----
;; base : bigint?
;;   底数。
;; exponent : bigint?
;;   整数指数；为负时要求 base 与模数互素。
;; m : bigint?
;;   正模数。
;;
;; 返回值
;; ----
;; bigint?
;;   模幂结果。
;;
;; 错误处理
;; ----
;; type-error / value-error
;;   参数类型错误时抛出 type-error；模数非正或负指数不可逆时抛出 value-error。

(import (liii check)
        (liii bigint)
) ;import

(check-set-mode! 'report-failed)
(define (b str)
  (string->bigint str)
) ;define

(check (bigint->string (bigint-mod-expt (b "3") (b "4") (b "5"))) => "1")
(check (bigint->string (bigint-mod-expt (b "2") bigint-zero (b "5"))) => "1")
(check (bigint->string (bigint-mod-expt (b "-2") (b "5") (b "7"))) => "3")
(check (bigint->string (bigint-mod-expt (b "3") (b "-1") (b "7"))) => "5")
(check (bigint->string (bigint-mod-expt (b "3") (b "-2") (b "7"))) => "4")

(check-catch 'type-error (bigint-mod-expt 2 (b "5") (b "7")))
(check-catch 'value-error (bigint-mod-expt (b "6") (b "-1") (b "9")))
(check-catch 'value-error (bigint-mod-expt (b "2") (b "5") bigint-zero))
(check-catch 'value-error (bigint-mod-expt (b "2") (b "5") (b "-7")))

(check-report)
