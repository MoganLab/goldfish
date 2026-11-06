;; check-report
;; 输出当前累计的断言汇总结果。
;;
;; 语法
;; ----
;; (check-report)
;;
;; 参数
;; ----
;; 无
;; 该函数不接受任何参数。
;;
;; 返回值
;; ----
;; unspecified
;; 向标准输出打印当前检查统计信息。

(import (srfi srfi-78))

(check-set-mode! 'report-failed)
(check 1 => 1)

(check-report)
