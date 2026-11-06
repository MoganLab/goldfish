;; timestamp+/no-rules
;; 对时间戳做纯时钟运算，不考虑时区规则与闰秒调整。
;;
;; 语法
;; ----
;; (timestamp+/no-rules ts dt)
;;
;; 参数
;; ----
;; ts : timestamp?
;; 起始时间戳。
;;
;; dt : time-delta?
;; 待叠加的时间间隔，使用其时、分、秒字段。
;;
;; 返回值
;; ----
;; timestamp?
;; 叠加后的时间戳；日期部分按纯时钟进位，时区保持不变。
;;
;; 说明
;; ----
;; 本函数不做时区规则换算（如夏令时切换），也不处理年、月、
;; 周、天字段；需要日历语义的时间加法应使用 timestamp+。
;; 起始时刻落在闰秒（秒字段 >= 60）时，运算前后各修正一秒
;; 以保持普通时间线连续。

(import (liii check)
  (liii date-time timestamp)
  (liii date-time timezone)
  (liii date-time time-delta)
  (liii date-time internal timestamp-math)
) ;import

(check-set-mode! 'report-failed)

(define ts (make-timestamp 2026 1 2 23 30 0 (utc-timezone)))

;; 跨日进位：23:30 + 2 小时 45 分 = 次日 02:15
(check
  (timestamp->iso-8601 (timestamp+/no-rules ts (time-delta+ (hours-delta 2) (minutes-delta 45)))
  ) ;timestamp->iso-8601
  =>
  "2026-01-03T02:15:00Z"
) ;check

;; 秒级叠加保留时分秒
(check
  (timestamp->iso-8601 (timestamp+/no-rules (make-timestamp 2026 1 2 1 1 1 (utc-timezone))
                         (seconds-delta 3600)
                       ) ;timestamp+/no-rules
  ) ;timestamp->iso-8601
  =>
  "2026-01-02T02:01:01Z"
) ;check

;; 负向推移跨入前一天
(check
  (timestamp->iso-8601 (timestamp+/no-rules (make-timestamp 2026 1 2 0 30 0 (utc-timezone))
                         (time-delta-negate (hours-delta 1))
                       ) ;timestamp+/no-rules
  ) ;timestamp->iso-8601
  =>
  "2026-01-01T23:30:00Z"
) ;check

;; 时区保持不变
(check
  (timestamp->iso-8601 (timestamp+/no-rules (make-timestamp 2026 1 2 12 0 0 (tz-timezone "Europe/Vilnius"))
                         (hours-delta 1)
                       ) ;timestamp+/no-rules
  ) ;timestamp->iso-8601
  =>
  "2026-01-02T13:00:00+02:00"
) ;check

(check-report)
