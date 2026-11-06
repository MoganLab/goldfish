;; time-range-offset
;; 读取时间区间的 UTC 偏移字段。
;;
;; 语法
;; ----
;; (time-range-offset r)
;;
;; 参数
;; ----
;; r : time-range? 时间区间记录。
;;
;; 返回值
;; ----
;; time-delta? UTC 偏移。

(import (liii check)
  (liii date-time time-delta)
  (liii date-time internal database-types)
) ;import

(check-set-mode! 'report-failed)

(define r (time-range 100 200 (hours-delta 8) #t "CST"))

(check (time-range-offset r) => (hours-delta 8))

(check-report)
