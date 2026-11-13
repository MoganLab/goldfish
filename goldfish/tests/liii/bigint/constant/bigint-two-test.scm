;; bigint-two
;; bigint 二值常量。
;;
;; 语法
;; ----
;; bigint-two
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; bigint?
;;   表示数值 2 的 bigint。

(import (liii check)
        (liii bigint)
) ;import

(check-set-mode! 'report-failed)

(check (bigint->string bigint-two) => "2")
(check-true (bigint-positive? bigint-two))

(check-report)
