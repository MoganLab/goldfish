;; timestamp->iso-8601
;; 将时间戳转换为符合 ISO-8601 规范的字符串表示。
;;
;; 语法
;; ----
;; (timestamp->iso-8601 ts)
;;
;; 参数
;; ----
;; ts : timestamp?
;;
;; 返回值
;; ----
;; string?
;;

(import (liii check)
  (liii date-time time-delta)
  (liii date-time timezone)
  (liii date-time timestamp)
) ;import

(check-set-mode! 'report-failed)

(check (timestamp->iso-8601 (make-timestamp 2026 1 2 3 4 5 (tz-timezone "Europe/Vilnius"))
       ) ;timestamp->iso-8601
  =>
  "2026-01-02T03:04:05+02:00"
) ;check
(check (timestamp->iso-8601 (make-timestamp 2026 1 2 3 4 5 (tz-timezone "America/New_York"))
       ) ;timestamp->iso-8601
  =>
  "2026-01-02T03:04:05-05:00"
) ;check
(check (timestamp->iso-8601 (make-timestamp 2026 1 2 3 4 5 (utc-timezone)))
  =>
  "2026-01-02T03:04:05Z"
) ;check
(check (timestamp->iso-8601 (make-timestamp 2026 1 2 3 4 (+ 5 1/10) (utc-timezone)))
  =>
  "2026-01-02T03:04:05.1Z"
) ;check

(check-catch 'type-error (timestamp->iso-8601 "2026-01-02T03:04:05Z"))

(check-report)
