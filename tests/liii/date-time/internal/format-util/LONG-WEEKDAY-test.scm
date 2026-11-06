;; LONG-WEEKDAY
;; 星期全称向量，索引 0-6 对应周日到周六。
;;
;; 语法
;; ----
;; LONG-WEEKDAY
;;
;; 参数
;; ----
;; 无。
;;
;; 返回值
;; ----
;; vector? 7 个星期全称。

(import (liii check)
  (liii date-time time-delta)
  (liii date-time internal format-util)
) ;import

(check-set-mode! 'report-failed)

(check (vector-length LONG-WEEKDAY) => 7)
(check (vector-ref LONG-WEEKDAY 0) => "Sunday")
(check (vector-ref LONG-WEEKDAY 1) => "Monday")

(check-report)
