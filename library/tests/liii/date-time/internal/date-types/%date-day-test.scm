;; %date-day
;; 读取 date 记录的日字段。
;;
;; 语法
;; ----
;; (%date-day date)
;;
;; 参数
;; ----
;; date : date? 日期对象。
;;
;; 返回值
;; ----
;; integer? 日。

(import (liii check) (liii date-time internal date-types))

(check-set-mode! 'report-failed)

(check (%date-day (%make-date 2021 2 3)) => 3)
(check (%date-day (%make-date 2021 12 31)) => 31)

(check-report)
