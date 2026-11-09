;; true
;; SICP 风格的真值常量，等价于 `#t`。
;;
;; 语法
;; ----
;; true
;;
;; 参数
;; ----
;; 无
;; 该常量不接受任何参数。
;;
;; 返回值
;; ----
;; boolean?
;; 恒为 `#t`。

(import (srfi sicp) (liii check))

(check-set-mode! 'report-failed)

(check-true (equal? true #t))
(check-true (boolean? true))
(check-true true)

(check-report)
