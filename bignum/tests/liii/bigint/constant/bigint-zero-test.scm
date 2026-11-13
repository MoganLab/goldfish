;; bigint-zero
;; bigint 零值常量。
;;
;; 语法
;; ----
;; bigint-zero
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; bigint?
;;   表示数值 0 的 bigint。

(import (liii check)
        (liii bigint)
) ;import

(check-set-mode! 'report-failed)

(check (bigint->string bigint-zero) => "0")
(check-true (bigint-zero? bigint-zero))

(check-report)
