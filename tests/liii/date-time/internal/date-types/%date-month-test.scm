;; %date-month
;; 读取 date 记录的月份字段。
;;
;; 语法
;; ----
;; (%date-month date)
;;
;; 参数
;; ----
;; date : date? 日期对象。
;;
;; 返回值
;; ----
;; integer? 月份。

(import (liii check) (liii date-time internal date-types))

(check-set-mode! 'report-failed)

(check (%date-month (%make-date 2021 2 3)) => 2)
(check (%date-month (%make-date 2021 12 31)) => 12)

(check-report)
