;; tzrule-dst-offset
;; 读取规则的夏令时偏移字段。
;;
;; 语法
;; ----
;; (tzrule-dst-offset rule)
;;
;; 参数
;; ----
;; rule : tzrule? DST 规则记录。
;;
;; 返回值
;; ----
;; time-delta? 夏令时偏移。

(import (liii check)
  (liii date-time time-delta)
  (liii date-time internal tz-types)
  (liii date-time internal dst)
) ;import

(check-set-mode! 'report-failed)

(define DST-TRANS
  (tz-transition (tz-month-week-day 3 2 0) (tz-offset #f 2 0 0))
) ;define

(define STD-TRANS
  (tz-transition (tz-month-week-day 11 1 0) (tz-offset #f 2 0 0))
) ;define

(define rule
  (tzrule "EST" (hours-delta -5) DST-TRANS "EDT" (hours-delta -4) STD-TRANS)
) ;define

(check (tzrule-dst-offset rule) => (hours-delta -4))

(check-report)
