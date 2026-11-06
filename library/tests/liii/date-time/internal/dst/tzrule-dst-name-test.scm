;; tzrule-dst-name
;; 读取规则的夏令时缩写字段。
;;
;; 语法
;; ----
;; (tzrule-dst-name rule)
;;
;; 参数
;; ----
;; rule : tzrule? DST 规则记录。
;;
;; 返回值
;; ----
;; string? 夏令时缩写。

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

(check (tzrule-dst-name rule) => "EDT")

(check-report)
