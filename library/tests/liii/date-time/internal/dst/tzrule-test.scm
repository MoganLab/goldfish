;; tzrule
;; 构造 DST 规则记录。
;;
;; 语法
;; ----
;; (tzrule std-name std-offset dst-transition dst-name dst-offset std-transition)
;;
;; 参数
;; ----
;; std-name : string? 标准时间缩写。
;; std-offset : time-delta? 标准时间偏移。
;; dst-transition : tz-transition? 进入夏令时转移。
;; dst-name : string? 夏令时缩写。
;; dst-offset : time-delta? 夏令时偏移。
;; std-transition : tz-transition? 退出夏令时转移。
;;
;; 返回值
;; ----
;; tzrule? 新建的规则记录。

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

(check-report)
