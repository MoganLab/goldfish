;; bigint-mod-inverse
;; 返回 a 在模 m 下的乘法逆元。
;;
;; 语法
;; ----
;; (bigint-mod-inverse a m)
;;
;; 参数
;; ----
;; a : bigint?
;;   待求逆的值。
;; m : bigint?
;;   正模数。
;;
;; 返回值
;; ----
;; bigint?
;;   落在 [0, m-1] 范围内的模逆元。
;;
;; 错误处理
;; ----
;; type-error / value-error
;;   参数类型错误时抛出 type-error；模数非正或逆元不存在时抛出 value-error。

(import (liii check)
        (liii bigint)
) ;import

(check-set-mode! 'report-failed)
(define (b str)
  (string->bigint str)
) ;define

(check (bigint->string (bigint-mod-inverse (b "3") (b "7"))) => "5")
(check (bigint->string (bigint-mod-inverse (b "-3") (b "7"))) => "2")
(check (bigint->string (bigint-mod-inverse (b "10") (b "17"))) => "12")

(check-catch 'type-error (bigint-mod-inverse 3 (b "7")))
(check-catch 'value-error (bigint-mod-inverse (b "6") (b "9")))
(check-catch 'value-error (bigint-mod-inverse (b "3") bigint-zero))
(check-catch 'value-error (bigint-mod-inverse (b "3") (b "-7")))

(check-report)
