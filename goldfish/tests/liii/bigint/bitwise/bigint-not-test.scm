;; bigint-not
;; 返回 bigint 的按位非结果。
;;
;; 语法
;; ----
;; (bigint-not n)
;;
;; 参数
;; ----
;; n : bigint?
;; 输入值。
;;
;; 返回值
;; ----
;; bigint?
;; 按位非后的结果。
;;
;; 错误处理
;; ----
;; type-error
;; 当参数不是 bigint 时抛出。

(import (liii check)
        (liii bigint bitwise)
        (liii bigint conversion)
) ;import

(check-set-mode! 'report-failed)
(check (bigint->string (bigint-not (string->bigint "0"))) => "-1")
(check (bigint->string (bigint-not (string->bigint "2"))) => "-3")
(check-catch 'type-error (bigint-not 1))

(check-report)
