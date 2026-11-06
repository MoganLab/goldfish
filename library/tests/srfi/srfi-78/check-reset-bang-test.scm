;; check-reset!
;; 重置内部的成功计数与失败记录列表。
;;
;; 语法
;; ----
;; (check-reset!)
;;
;; 参数
;; ----
;; 无
;; 该函数不接受任何参数。
;;
;; 返回值
;; ----
;; unspecified
;; 将内部成功计数重置为 0，并将失败列表清空。

(import (srfi srfi-78))

(check-set-mode! 'summary)
(check 1 => 2)
(check (check-failed?) => #t)

(check-reset!)
(check (check-failed?) => #f)

(check-report)
