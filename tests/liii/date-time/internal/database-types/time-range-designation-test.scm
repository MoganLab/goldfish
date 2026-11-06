;; time-range-designation
;; 读取时间区间的时区缩写字段。
;;
;; 语法
;; ----
;; (time-range-designation r)
;;
;; 参数
;; ----
;; r : time-range? 时间区间记录。
;;
;; 返回值
;; ----
;; string? 时区缩写。

(import (liii check)
  (liii date-time time-delta)
  (liii date-time internal database-types)
) ;import

(check-set-mode! 'report-failed)

(define r (time-range 100 200 (hours-delta 8) #t "CST"))

(check (time-range-designation r) => "CST")

(check-report)
