(import (liii check)
        (liii time)
        (srfi srfi-19)
) ;import

(check-set-mode! 'report-failed)

;; time-utc->date
;; 将 UTC 时间对象转换为日期对象。
;;
;; 语法
;; ----
;; (time-utc->date time [tz-offset])
;;
;; 参数
;; ----
;; time : time? TIME-UTC 类型的时间对象。
;; tz-offset : integer? (可选) 时区偏移（秒），默认为 0。
;;
;; 返回值
;; ----
;; date? 转换后的日期对象。
;;
;; 错误处理
;; --------
;; wrong-type-arg 当参数不是时间对象或类型不匹配时抛出错误。

;; time-utc->date basic (UTC)
(let* ((t (make-time TIME-UTC 0 0))
       (d (time-utc->date t 0)))
  (check (date-year d) => 1970)
  (check (date-month d) => 1)
  (check (date-day d) => 1)
  (check (date-hour d) => 0)
  (check (date-minute d) => 0)
  (check (date-second d) => 0)
  (check (date-zone-offset d) => 0)
) ;let*

;; time-utc->date with positive tz offset (+8)
(let* ((t (make-time TIME-UTC 0 0))
       (d (time-utc->date t 28800)))
  (check (date-year d) => 1970)
  (check (date-month d) => 1)
  (check (date-day d) => 1)
  (check (date-hour d) => 8)
  (check (date-minute d) => 0)
  (check (date-second d) => 0)
  (check (date-zone-offset d) => 28800)
) ;let*

;; Error conditions
(check-catch 'wrong-type-arg
  (time-utc->date (make-time TIME-TAI 0 0) 0)
) ;check-catch
(check-catch 'wrong-type-arg
  (time-utc->date "not-a-time" 0)
) ;check-catch

(check-report)
