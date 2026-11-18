;; bigint-prime?
;; 判断 bigint 是否为素数。
;;
;; 语法
;; ----
;; (bigint-prime? n)
;; (bigint-prime? n certainty)
;;
;; 参数
;; ----
;; n : bigint?
;;   待判断的整数。
;; certainty : integer?
;;   非负整数，表示可接受的不确定性参数。
;;
;; 返回值
;; ----
;; boolean?
;;   可能为素数时返回 #t，确定为合数时返回 #f。
;;
;; 错误处理
;; ----
;; type-error / value-error
;;   参数类型错误时抛出 type-error；certainty 为负数时抛出 value-error。

(import (liii check)
        (liii bigint)
) ;import

(check-set-mode! 'report-failed)
(define (b str)
  (string->bigint str)
) ;define

(check-false (bigint-prime? (b "-7")))
(check-false (bigint-prime? bigint-zero))
(check-false (bigint-prime? bigint-one))
(check-true (bigint-prime? bigint-two))
(check-true (bigint-prime? (b "3")))
(check-true (bigint-prime? (b "17")))
(check-false (bigint-prime? (b "18")))
(check-false (bigint-prime? (b "561")))
(check-false (bigint-prime? (b "1105")))
(check-true (bigint-prime? (b "2147483647")))
(check-true (bigint-prime? (b "17") 0))

(check-catch 'type-error (bigint-prime? 17))
(check-catch 'type-error (bigint-prime? (b "17") "10"))
(check-catch 'value-error (bigint-prime? (b "17") -1))

(check-report)
