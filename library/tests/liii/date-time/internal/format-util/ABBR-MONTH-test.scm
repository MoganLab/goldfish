;; ABBR-MONTH
;; 月份缩写向量，索引 1-12 对应一月到十二月，索引 0 为空串。
;;
;; 语法
;; ----
;; ABBR-MONTH
;;
;; 参数
;; ----
;; 无。
;;
;; 返回值
;; ----
;; vector? 13 个字符串，索引 0 为占位空串。

(import (liii check)
  (liii date-time time-delta)
  (liii date-time internal format-util)
) ;import

(check-set-mode! 'report-failed)

(check (vector-length ABBR-MONTH) => 13)
(check (vector-ref ABBR-MONTH 0) => "")
(check (vector-ref ABBR-MONTH 1) => "Jan")
(check (vector-ref ABBR-MONTH 12) => "Dec")

(check-report)
