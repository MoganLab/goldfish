;; format-tz-offset
;; 将 time-delta 偏移格式化为 +HHMM 或 +HH:MM 字符串。
;;
;; 语法
;; ----
;; (format-tz-offset off with-colon?)
;;
;; 参数
;; ----
;; off : time-delta? 时区偏移增量。
;; with-colon? : boolean? 是否在时分之间插入冒号。
;;
;; 返回值
;; ----
;; string? 带符号的偏移字符串。

(import (liii check)
  (liii date-time time-delta)
  (liii date-time internal format-util)
) ;import

(check-set-mode! 'report-failed)

(check (format-tz-offset (time-delta+ (hours-delta 5) (minutes-delta 30)) #t)
  =>
  "+05:30"
) ;check
(check (format-tz-offset (hours-delta -8) #f) => "-0800")
(check (format-tz-offset (make-time-delta 0 0 0 0 0 0 0) #t) => "+00:00")

(check-report)
