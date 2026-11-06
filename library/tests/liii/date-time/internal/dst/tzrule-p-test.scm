;; tzrule?
;; 判断对象是否为 DST 规则记录。
;;
;; 语法
;; ----
;; (tzrule? rule)
;;
;; 参数
;; ----
;; rule : tzrule? DST 规则记录。
;;
;; 返回值
;; ----
;; boolean? 是该类记录返回 #t。

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

(check-true (tzrule? rule))
(check-false (tzrule? "rule"))

(check-report)
