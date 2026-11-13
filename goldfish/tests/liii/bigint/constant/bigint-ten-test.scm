;; bigint-ten
;; bigint 十值常量。
;;
;; 语法
;; ----
;; bigint-ten
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; bigint?
;;   表示数值 10 的 bigint。

(import (liii check)
        (liii bigint)
) ;import

(check-set-mode! 'report-failed)

(check (bigint->string bigint-ten) => "10")
(check-false (bigint-negative? bigint-ten))

(check-report)
