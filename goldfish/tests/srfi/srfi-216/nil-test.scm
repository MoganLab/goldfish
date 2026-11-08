;; nil
;; SICP 风格的空列表常量，等价于 `'()`。
;;
;; 语法
;; ----
;; nil
;;
;; 参数
;; ----
;; 无
;; 该常量不接受任何参数。
;;
;; 返回值
;; ----
;; list?
;; 恒为空列表 `'()`。

(import (srfi sicp) (liii check))

(check-set-mode! 'report-failed)

(check-true (null? nil))
(check-true (equal? nil '()))
(check-true (list? nil))

(check-report)
