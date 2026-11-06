;; timestamp-timezone-offset
;; 获取时间戳在当前时区下的 UTC 偏移量（作为 time-delta 返回）。
;;
;; 语法
;; ----
;; (timestamp-timezone-offset ts)
;;
;; 参数
;; ----
;; ts : timestamp?
;;
;; 返回值
;; ----
;; time-delta?

(import (liii check)
  (liii date-time time-delta)
  (liii date-time timezone)
  (liii date-time timestamp)
) ;import

(check-set-mode! 'report-failed)


(let* ((t1 (make-timestamp 2026 2 3 5 0 0 (tz-timezone "Europe/Vilnius")))
       (off1 (timestamp-timezone-offset t1))
       (t2 (make-timestamp 2026 2 2 22 0 0 (tz-timezone "America/New_York")))
       (off2 (timestamp-timezone-offset t2))
      ) ;
  (check (time-delta-hours off1) => 2)
  (check (time-delta-hours off2) => -5)
) ;let*

(check-catch 'type-error (timestamp-timezone-offset "invalid"))

(check-report)
