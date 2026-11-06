;; ABBR-WEEKDAY
;; 星期名称缩写向量，索引 0-6 对应周日到周六。
;;
;; 语法
;; ----
;; ABBR-WEEKDAY
;;
;; 参数
;; ----
;; 无。
;;
;; 返回值
;; ----
;; vector? 7 个三字星期缩写。

(import (liii check)
  (liii date-time time-delta)
  (liii date-time internal format-util)
) ;import

(check-set-mode! 'report-failed)

(check (vector-length ABBR-WEEKDAY) => 7)
(check (vector-ref ABBR-WEEKDAY 0) => "Sun")
(check (vector-ref ABBR-WEEKDAY 6) => "Sat")

(check-report)
