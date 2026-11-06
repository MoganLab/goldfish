;; %date-year
;; 读取 date 记录的年份字段。
;;
;; 语法
;; ----
;; (%date-year date)
;;
;; 参数
;; ----
;; date : date? 日期对象。
;;
;; 返回值
;; ----
;; integer? 年份。

(import (liii check) (liii date-time internal date-types))

(check-set-mode! 'report-failed)

(check (%date-year (%make-date 2021 2 3)) => 2021)
(check (%date-year (%make-date -44 3 15)) => -44)

(check-report)
