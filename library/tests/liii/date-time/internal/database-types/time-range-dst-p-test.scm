;; time-range-dst?
;; 读取时间区间的夏令时标志字段。
;;
;; 语法
;; ----
;; (time-range-dst? r)
;;
;; 参数
;; ----
;; r : time-range? 时间区间记录。
;;
;; 返回值
;; ----
;; boolean? 处于夏令时为 #t。

(import (liii check)
  (liii date-time time-delta)
  (liii date-time internal database-types)
) ;import

(check-set-mode! 'report-failed)

(define r (time-range 100 200 (hours-delta 8) #t "CST"))

(check (time-range-dst? r) => #t)
(check (time-range-dst? (time-range 0 1 (hours-delta 0) #f "UTC")) => #f)

(check-report)
