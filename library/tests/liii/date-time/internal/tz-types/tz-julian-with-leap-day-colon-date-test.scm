;; tz-julian-with-leap-day:date
;; 读取考虑闰日的儒略日记录的日序字段。
;;
;; 语法
;; ----
;; tz-julian-with-leap-day:date jd
;;
;; 参数
;; ----
;; jd : tz-julian-with-leap-day? 转移日期记录。
;;
;; 返回值
;; ----
;; integer? 0-based 年内日序。

(import (liii check) (liii date-time internal tz-types))

(check-set-mode! 'report-failed)

(define jd (tz-julian-with-leap-day 59))

(check (tz-julian-with-leap-day:date jd) => 59)

(check-report)
