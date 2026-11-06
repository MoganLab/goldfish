;; date+clock-time->timestamp
;; 由日期、时钟时间与时区组装时间戳，并校验本地时间在时区内合法。
;;
;; 语法
;; ----
;; (date+clock-time->timestamp date time timezone)
;;
;; 参数
;; ----
;; date : date? 日期对象。
;; time : clock-time? 时钟时间对象。
;; timezone : timezone? 时区对象。
;;
;; 返回值
;; ----
;; timestamp? 组装得到的时间戳。

(import (liii check)
  (liii date-time date)
  (liii date-time clock-time)
  (liii date-time time-delta)
  (liii date-time timezone)
  (liii date-time timestamp)
  (liii date-time internal timestamp-construct)
) ;import

(check-set-mode! 'report-failed)

(define ts
  (date+clock-time->timestamp (make-date 2021 1 2)
    (make-clock-time 3 4 5)
    (utc-timezone)
  ) ;date+clock-time->timestamp
) ;define

(check (list (timestamp-year ts)
         (timestamp-month ts)
         (timestamp-day ts)
         (timestamp-hour ts)
         (timestamp-minute ts)
         (timestamp-second ts)
       ) ;list
  =>
  '(2021 1 2 3 4 5)
) ;check
(check (timestamp-fold (date+clock-time->timestamp (make-date 2021 11 7)
                         (make-clock-time 1 30 0)
                         (tz-timezone "America/New_York")
                         1
                       ) ;date+clock-time->timestamp
       ) ;timestamp-fold
  =>
  1
) ;check
(check-catch 'type-error
  (date+clock-time->timestamp (make-date 2021 3 14)
    (make-clock-time 2 30 0)
    (tz-timezone "America/New_York")
  ) ;date+clock-time->timestamp
) ;check-catch
(check-catch 'type-error
  (date+clock-time->timestamp "date" (make-clock-time 0 0 0) (utc-timezone))
) ;check-catch

(check-report)
