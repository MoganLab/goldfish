;; time-range
;; 构造时区时间区间记录。
;;
;; 语法
;; ----
;; (time-range start end offset dst? designation)
;;
;; 参数
;; ----
;; start : integer? 或 #f 区间起点（含）。
;; end : integer? 或 #f 区间终点（不含）。
;; offset : time-delta? UTC 偏移。
;; dst? : boolean? 是否夏令时。
;; designation : string? 时区缩写。
;;
;; 返回值
;; ----
;; time-range? 新建的时间区间记录。

(import (liii check)
  (liii date-time time-delta)
  (liii date-time internal database-types)
) ;import

(check-set-mode! 'report-failed)

(define r (time-range 100 200 (hours-delta 8) #t "CST"))

(check-true (time-range? r))

(check-report)
