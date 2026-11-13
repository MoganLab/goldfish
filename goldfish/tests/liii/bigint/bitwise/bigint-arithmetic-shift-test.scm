;; bigint-arithmetic-shift
;; 对 bigint 做算术移位。
;;
;; 语法
;; ----
;; (bigint-arithmetic-shift a n)
;;
;; 参数
;; ----
;; a : bigint?
;; 输入值。
;; n : integer?
;; 移位位数，正数左移，负数右移。
;;
;; 返回值
;; ----
;; bigint?
;; 移位后的结果。
;;
;; 错误处理
;; ----
;; type-error
;; 当第一个参数不是 bigint 时抛出。

(import (liii check)
        (liii bigint bitwise)
        (liii bigint conversion)
) ;import

(check-set-mode! 'report-failed)
(check (bigint->string (bigint-arithmetic-shift (string->bigint "3") 2))
       => "12"
) ;check
(check (bigint->string (bigint-arithmetic-shift (string->bigint "12") -2))
       => "3"
) ;check
(check (bigint->string (bigint-arithmetic-shift (string->bigint "-3") -1))
       => "-2"
) ;check
(check (bigint->string (bigint-arithmetic-shift (string->bigint "-8") -1))
       => "-4"
) ;check
(check-catch 'type-error (bigint-arithmetic-shift 3 1))

(check-report)
