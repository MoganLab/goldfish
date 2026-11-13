;; bigint-subtract
;; 计算两个 bigint 的差。
;;
;; 语法
;; ----
;; (bigint-subtract a b)
;;
;; 参数
;; ----
;; a : bigint?
;;   被减数。
;; b : bigint?
;;   减数。
;;
;; 返回值
;; ----
;; bigint?
;;   a 减 b 的结果。
;;
;; 错误处理
;; ----
;; type-error
;;   当任一参数不是 bigint 时抛出。

(import (liii check)
        (liii bigint)
) ;import

(check-set-mode! 'report-failed)
(define (b str)
  (string->bigint str)
) ;define

(check (bigint->string (bigint-subtract (b "1000") (b "1"))) => "999")
(check (bigint->string (bigint-subtract (b "1") (b "1000"))) => "-999")
(check (bigint->string (bigint-subtract (b "-5") (b "-2"))) => "-3")
(check (bigint->string (bigint-subtract (b "4294967296") (b "1"))) => "4294967295")
(check (bigint->string (bigint-subtract (b "1000000000000000000000000000000")
                                        (b "1")))
       => "999999999999999999999999999999"
) ;check

(check-catch 'type-error (bigint-subtract 1 (b "2")))
(check-catch 'type-error (bigint-subtract (b "1") 2))

(check-report)
