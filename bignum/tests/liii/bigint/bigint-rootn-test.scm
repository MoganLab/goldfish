;; bigint-rootn
;; 返回不超过实数 n 次根的最大整数根。
;;
;; 语法
;; ----
;; (bigint-rootn n degree)
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
;; bigint?
;;   整数 n 次根。
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

(check (bigint->string (bigint-rootn (b "27") 3)) => "3")
(check (bigint->string (bigint-rootn (b "28") 3)) => "3")
(check (bigint->string (bigint-rootn (b "-27") 3)) => "-3")
(check (bigint->string (bigint-rootn (b "16") 4)) => "2")

(check-catch 'type-error (bigint-rootn 27 3))
(check-catch 'type-error (bigint-rootn (b "27") "3"))
(check-catch 'value-error (bigint-rootn (b "27") 0))
(check-catch 'value-error (bigint-rootn (b "-16") 2))

(check-report)
