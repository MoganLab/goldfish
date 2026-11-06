;; timezone-first
;; 读取时区记录的首个区间字段。
;;
;; 语法
;; ----
;; (timezone-first tz)
;;
;; 参数
;; ----
;; tz : timezone? 时区记录。
;;
;; 返回值
;; ----
;; time-range? 或 #f 首个区间。

(import (liii check)
  (liii date-time time-delta)
  (liii date-time internal database-types)
) ;import

(check-set-mode! 'report-failed)

(define r (time-range 100 200 (hours-delta 8) #t "CST"))

(define utc-v (vector r))

(define loc-v (vector r))

(define tz (make-timezone r utc-v loc-v #f))

(check (timezone-first tz) => r)

(check-report)
