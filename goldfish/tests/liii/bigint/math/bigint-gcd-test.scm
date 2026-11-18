;; bigint-gcd
;; 计算两个 bigint 的最大公约数，结果恒为非负。
;;
;; 语法
;; ----
;; (bigint-gcd a b)
;;
;; 参数
;; ----
;; a : bigint?
;;   第一个操作数。
;; b : bigint?
;;   第二个操作数。
;;
;; 返回值
;; ----
;; bigint?
;;   最大公约数。
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

(check (bigint->string (bigint-gcd (b "48") (b "18"))) => "6")
(check (bigint->string (bigint-gcd (b "-48") (b "18"))) => "6")
(check (bigint->string (bigint-gcd bigint-zero (b "18"))) => "18")
(check (bigint->string (bigint-gcd bigint-zero bigint-zero)) => "0")

(check-catch 'type-error (bigint-gcd 1 (b "2")))

(check-report)
