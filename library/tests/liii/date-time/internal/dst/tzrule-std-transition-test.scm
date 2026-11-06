;; tzrule-std-transition
;; 读取规则退出夏令时的转移字段。
;;
;; 语法
;; ----
;; (tzrule-std-transition rule)
;;
;; 参数
;; ----
;; rule : tzrule? DST 规则记录。
;;
;; 返回值
;; ----
;; tz-transition? 退出夏令时的转移。

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

(check (tzrule-std-transition rule) => STD-TRANS)

(check-report)
