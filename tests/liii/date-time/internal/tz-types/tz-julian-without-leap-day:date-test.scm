;; tz-julian-without-leap-day:date
;; 读取忽略闰日的儒略日记录的日序字段。
;;
;; 语法
;; ----
;; tz-julian-without-leap-day:date jd
;;
;; 参数
;; ----
;; jd : tz-julian-without-leap-day? 转移日期记录。
;;
;; 返回值
;; ----
;; integer? 1-based 年内日序。

(import (liii check) (liii date-time internal tz-types))

(check-set-mode! 'report-failed)

(define jd (tz-julian-without-leap-day 60))

(check (tz-julian-without-leap-day:date jd) => 60)

(check-report)
