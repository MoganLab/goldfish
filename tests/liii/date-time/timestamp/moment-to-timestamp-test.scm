(import (liii check)
        (liii date-time date)
        (liii date-time moment)
        (liii date-time timezone)
        (liii date-time timestamp))

(check-set-mode! 'report-failed)

;; moment->timestamp
;; 将时刻（moment）对象转换为指定时区下的时间戳（timestamp）。
;;
;; 语法
;; ----
;; (moment->timestamp moment timezone)
;;
;; 参数
;; ----
;; moment : moment?
;; timezone : timezone?
;;
;; 返回值
;; ----
;; timestamp?
;;
;; 示例
;; ----
;; (moment->timestamp (make-moment (make-date 2026 2 3) 3723) (tz-timezone "Europe/Vilnius"))

(let* ((moment (make-moment (make-date 2026 2 3) (+ (* 1 3600) (* 2 60) 3 37)))
       (ts (moment->timestamp moment (tz-timezone "Europe/Vilnius"))))
  (check-true (timestamp? ts))
  (check (timestamp-year ts) => 2026)
  (check (timestamp-month ts) => 2)
  (check (timestamp-day ts) => 3)
  (check (timestamp-hour ts) => 3)
  (check (timestamp-minute ts) => 2)
  (check (timestamp-second ts) => 3))

(check-catch 'type-error (moment->timestamp "not-a-moment" (utc-timezone)))
(check-catch 'type-error (moment->timestamp (make-moment (make-date 2020 1 1) 0) "UTC"))

(check-report)
