;; bigint-next-prime
;; 返回严格大于输入值的下一个素数。
;;
;; 语法
;; ----
;; (bigint-next-prime n)
;;
;; 参数
;; ----
;; n : bigint?
;;   起始值。
;;
;; 返回值
;; ----
;; bigint?
;;   大于 n 的第一个素数。
;;
;; 错误处理
;; ----
;; type-error
;;   当参数不是 bigint 时抛出。

(import (liii check)
        (liii bigint)
) ;import

(check-set-mode! 'report-failed)
(define (b str)
  (string->bigint str)
) ;define

(check (bigint->string (bigint-next-prime (b "-100"))) => "2")
(check (bigint->string (bigint-next-prime bigint-zero)) => "2")
(check (bigint->string (bigint-next-prime bigint-one)) => "2")
(check (bigint->string (bigint-next-prime bigint-two)) => "3")
(check (bigint->string (bigint-next-prime (b "14"))) => "17")
(check (bigint->string (bigint-next-prime (b "100"))) => "101")

(check-catch 'type-error (bigint-next-prime 100))

(check-report)
