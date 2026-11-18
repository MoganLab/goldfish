;; bigint-negate
;; 返回 bigint 的相反数。
;;
;; 语法
;; ----
;; (bigint-negate n)
;;
;; 参数
;; ----
;; n : bigint?
;;   输入 bigint。
;;
;; 返回值
;; ----
;; bigint?
;;   n 的相反数。
;;
;; 错误处理
;; ----
;; type-error
;;   当参数不是 bigint 时抛出。

(import (liii check)
        (liii bigint)
) ;import

(check-set-mode! 'report-failed)
(check (bigint->string (bigint-negate bigint-zero)) => "0")
(check (bigint->string (bigint-negate (string->bigint "123"))) => "-123")
(check (bigint->string (bigint-negate (string->bigint "-456"))) => "456")

(check-catch 'type-error (bigint-negate 123))

(check-report)
