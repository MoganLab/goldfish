(import (liii check)
        (liii date-time))

(check-set-mode! 'report-failed)

;; (liii date-time) 总聚合入口测试
(let* ((d (make-date 2026 2 3))
       (ct (make-clock-time 15 30 45))
       (tz (utc-timezone))
       (ts (date+clock-time->timestamp d ct tz))
       (m (timestamp->moment ts))
       (ts2 (moment->timestamp m tz))
       (dt (hours-delta 2))
       (ts3 (timestamp+ ts dt)))
  (check-true (date? d))
  (check-true (clock-time? ct))
  (check-true (timezone? tz))
  (check-true (timestamp? ts))
  (check-true (moment? m))
  (check-true (timestamp=? ts ts2))
  (check (timestamp-hour ts3) => 17)
  (check (date->iso-8601 d) => "2026-02-03")
  (check (clock-time->iso-8601 ct) => "15:30:45")
  (check (timestamp->iso-8601 ts) => "2026-02-03T15:30:45Z"))

;; current-time 接口
(check-true (moment? (current-moment)))
(check-true (timestamp? (current-utc-timestamp)))
(check-true (timestamp? (current-system-timestamp)))

;; format 接口聚合导出测试
(let* ((ts (make-timestamp 2026 2 3 15 30 45 (utc-timezone)))
       (fmt-str (date-time-format ts "%Y/%m/%d %H:%M:%S"))
       (parsed-ts (date-time-parse fmt-str "%Y/%m/%d %H:%M:%S" (utc-timezone))))
  (check fmt-str => "2026/02/03 15:30:45")
  (check (timestamp=? ts parsed-ts) => #t))

(check-report)
