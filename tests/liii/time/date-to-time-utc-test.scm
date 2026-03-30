(import (liii check)
        (liii time)
        (srfi srfi-19)
) ;import

(check-set-mode! 'report-failed)

;; date->time-utc
;; 将日期对象转换为 UTC 时间对象。
;;
;; 语法
;; ----
;; (date->time-utc date)
;;
;; 参数
;; ----
;; date : date? 要转换的日期对象。
;;
;; 返回值
;; ----
;; time? TIME-UTC 类型的时间对象。
;;
;; 错误处理
;; --------
;; wrong-type-arg 当参数不是日期对象时抛出错误。

;; date->time-utc basic
(let* ((d (make-date 0 0 0 8 1 1 1970 28800))
       (t (date->time-utc d)))
  (check (time-type t) => TIME-UTC)
  (check (time-second t) => 0)
  (check (time-nanosecond t) => 0)
) ;let*

;; round-trip date -> time -> date with same tz-offset
(let* ((d1 (make-date 123456789 45 30 14 25 12 2023 28800))
       (t (date->time-utc d1))
       (d2 (time-utc->date t (date-zone-offset d1))))
  (check (date-year d2) => (date-year d1))
  (check (date-month d2) => (date-month d1))
  (check (date-day d2) => (date-day d1))
  (check (date-hour d2) => (date-hour d1))
  (check (date-minute d2) => (date-minute d1))
  (check (date-second d2) => (date-second d1))
  (check (date-nanosecond d2) => (date-nanosecond d1))
  (check (date-zone-offset d2) => (date-zone-offset d1))
) ;let*

;; Error conditions
(check-catch 'wrong-type-arg
  (date->time-utc "not-a-date")
) ;check-catch

(check-report)
