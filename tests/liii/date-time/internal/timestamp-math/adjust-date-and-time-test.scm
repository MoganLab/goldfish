;; adjust-date-and-time
;; 对时间戳应用日历语义的时间调整，返回新日期与新时钟。
;;
;; 语法
;; ----
;; (adjust-date-and-time ts-utc dt)
;;
;; 参数
;; ----
;; ts-utc : timestamp?
;; 以 UTC 表示的起始时间戳。
;;
;; dt : time-delta?
;; 待叠加的时间间隔，年、月、周、天字段作用于日期，
;; 时、分字段作用于时钟。
;;
;; 返回值
;; ----
;; values date? clock-time? clock-time?
;; 三个值：调整后的日期、调整后的时钟、调整前的时钟。
;;
;; 说明
;; ----
;; 日期部分按日历加法（年/月/周/天）计算，时钟部分按分钟数
;; 相加并折算跨日；秒字段原样保留。第三个返回值供调用方
;; 判断时钟是否发生过回绕。

(import (liii check)
  (liii date-time clock-time)
  (liii date-time date)
  (liii date-time timestamp)
  (liii date-time timezone)
  (liii date-time time-delta)
  (liii date-time internal timestamp-math)
) ;import

(check-set-mode! 'report-failed)

(define ts (make-timestamp 2026 1 31 23 30 45 (utc-timezone)))

;; 1 天 + 2 小时：日期 +2 天，时钟回绕到 01:30:45
(call-with-values
  (lambda ()
    (adjust-date-and-time ts (time-delta+ (days-delta 1) (hours-delta 2)))
  ) ;lambda
  (lambda (d3 clk2 clk)
    (check (date->iso-8601 d3) => "2026-02-02")
    (check (clock-time->iso-8601 clk2) => "01:30:45")
    (check (clock-time->iso-8601 clk) => "23:30:45")
  ) ;lambda
) ;call-with-values

;; 月份调整：2026-01-15 + 1 个月 = 2026-02-15
(call-with-values
  (lambda ()
    (adjust-date-and-time (make-timestamp 2026 1 15 8 0 0 (utc-timezone))
      (months-delta 1)
    ) ;adjust-date-and-time
  ) ;lambda
  (lambda (d3 clk2 clk)
    (check (date->iso-8601 d3) => "2026-02-15")
    (check (clock-time->iso-8601 clk2) => "08:00:00")
  ) ;lambda
) ;call-with-values

;; 空间隔不改变日期与时钟
(call-with-values (lambda () (adjust-date-and-time ts (make-time-delta 0 0 0 0 0 0 0)))
  (lambda (d3 clk2 clk)
    (check (date->iso-8601 d3) => "2026-01-31")
    (check (clock-time->iso-8601 clk2) => "23:30:45")
  ) ;lambda
) ;call-with-values

(check-report)
