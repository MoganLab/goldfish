;; tz-julian-with-leap-day
;; 构造考虑闰日的儒略日转移记录（0-based 年内日序）。
;;
;; 语法
;; ----
;; tz-julian-with-leap-day date
;;
;; 参数
;; ----
;; date : integer? 年内日序（0-365）。
;;
;; 返回值
;; ----
;; tz-julian-with-leap-day? 新建的转移日期记录。

(import (liii check) (liii date-time internal tz-types))

(check-set-mode! 'report-failed)

(define jd (tz-julian-with-leap-day 59))

(check-true (tz-julian-with-leap-day? jd))

(check-report)
