;; tz-julian-without-leap-day
;; 构造忽略闰日的儒略日转移记录（1-based 年内日序）。
;;
;; 语法
;; ----
;; tz-julian-without-leap-day date
;;
;; 参数
;; ----
;; date : integer? 年内日序（1-365）。
;;
;; 返回值
;; ----
;; tz-julian-without-leap-day? 新建的转移日期记录。

(import (liii check) (liii date-time internal tz-types))

(check-set-mode! 'report-failed)

(define jd (tz-julian-without-leap-day 60))

(check-true (tz-julian-without-leap-day? jd))

(check-report)
