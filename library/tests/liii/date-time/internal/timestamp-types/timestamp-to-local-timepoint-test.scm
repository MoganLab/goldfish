;; timestamp->local-timepoint
;; 将时间戳的本地日期时间换算为自 UNIX 纪元起的秒数（忽略时区偏移）。
;;
;; 语法
;; ----
;; (timestamp->local-timepoint ts)
;;
;; 参数
;; ----
;; ts : timestamp? 时间戳对象。
;;
;; 返回值
;; ----
;; integer? 本地时间点秒数，早于纪元时为负。

(import (liii check)
  (liii date-time date)
  (liii date-time clock-time)
  (liii date-time timezone)
  (liii date-time internal timestamp-types)
) ;import

(check-set-mode! 'report-failed)

(check (timestamp->local-timepoint (%make-timestamp (make-date 1970 1 1) (make-clock-time 1 2 3) (utc-timezone) 0)
       ) ;timestamp->local-timepoint
  =>
  3723
) ;check
(check (timestamp->local-timepoint (%make-timestamp (make-date 1970 1 2) (make-clock-time 0 0 0) (utc-timezone) 0)
       ) ;timestamp->local-timepoint
  =>
  86400
) ;check
(check (timestamp->local-timepoint (%make-timestamp (make-date 1969 12 31)
                                     (make-clock-time 23 59 59)
                                     (utc-timezone)
                                     0
                                   ) ;%make-timestamp
       ) ;timestamp->local-timepoint
  =>
  -1
) ;check

(check-report)
