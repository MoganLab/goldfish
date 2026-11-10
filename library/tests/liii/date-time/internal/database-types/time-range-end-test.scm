;; time-range-end
;; 读取时间区间的终点字段。
;;
;; 语法
;; ----
;; (time-range-end r)
;;
;; 参数
;; ----
;; r : time-range? 时间区间记录。
;;
;; 返回值
;; ----
;; integer? 或 #f 区间终点。

(import (liii check)
  (liii date-time time-delta)
  (liii date-time internal database-types)
) ;import

(check-set-mode! 'report-failed)

(define r (time-range 100 200 (hours-delta 8) #t "CST"))

(check (time-range-end r) => 200)

(check-report)
