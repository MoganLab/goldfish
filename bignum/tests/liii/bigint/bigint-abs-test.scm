;; bigint-abs
;; 返回 bigint 的绝对值。
;;
;; 语法
;; ----
;; (bigint-abs n)
;;
;; 参数
;; ----
;; n : bigint?
;;   输入 bigint。
;;
;; 返回值
;; ----
;; bigint?
;;   n 的绝对值。
;;
;; 错误处理
;; ----
;; type-error
;;   当参数不是 bigint 时抛出。

(import (liii check)
        (liii bigint)
) ;import

(check-set-mode! 'report-failed)
(check (bigint->string (bigint-abs bigint-zero)) => "0")
(check (bigint->string (bigint-abs (string->bigint "123"))) => "123")
(check (bigint->string (bigint-abs (string->bigint "-456"))) => "456")

(check-catch 'type-error (bigint-abs 123))

(check-report)
