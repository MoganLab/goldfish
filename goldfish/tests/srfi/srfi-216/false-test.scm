;; false
;; SICP 风格的假值常量，等价于 `#f`。
;;
;; 语法
;; ----
;; false
;;
;; 参数
;; ----
;; 无
;; 该常量不接受任何参数。
;;
;; 返回值
;; ----
;; boolean?
;; 恒为 `#f`。

(import (srfi sicp) (liii check))

(check-set-mode! 'report-failed)

(check-true (equal? false #f))
(check-true (boolean? false))
(check-false false)

(check-report)
