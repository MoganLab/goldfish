;; bigint-one
;; bigint 一值常量。
;;
;; 语法
;; ----
;; bigint-one
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; bigint?
;;   表示数值 1 的 bigint。

(import (liii check)
        (liii bigint)
) ;import

(check-set-mode! 'report-failed)

(check (bigint->string bigint-one) => "1")
(check-true (bigint-positive? bigint-one))

(check-report)
