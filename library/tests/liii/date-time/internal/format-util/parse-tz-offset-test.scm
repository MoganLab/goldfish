;; parse-tz-offset
;; 解析 ISO-8601 时区偏移（Z、+HHMM、+HH:MM），返回增量与新位置。
;;
;; 语法
;; ----
;; (parse-tz-offset input-str pos)
;;
;; 参数
;; ----
;; input-str : string? 输入串。
;; pos : integer? 偏移起始位置。
;;
;; 返回值
;; ----
;; values time-delta? 偏移量与 integer? 结束位置；格式非法抛 value-error。

(import (liii check)
  (liii date-time time-delta)
  (liii date-time internal format-util)
) ;import

(check-set-mode! 'report-failed)

(check (call-with-values (lambda () (parse-tz-offset "Z" 0)) list)
  =>
  (list (make-time-delta 0 0 0 0 0 0 0) 1)
) ;check
(check (call-with-values (lambda () (parse-tz-offset "+05:30" 0)) list)
  =>
  (list (time-delta+ (hours-delta 5) (minutes-delta 30)) 6)
) ;check
(check (call-with-values (lambda () (parse-tz-offset "-0800" 0)) list)
  =>
  (list (time-delta-negate (time-delta+ (hours-delta 8))) 5)
) ;check
(check-catch 'value-error (parse-tz-offset "" 0))
(check-catch 'value-error (parse-tz-offset "X" 0))

(check-report)
