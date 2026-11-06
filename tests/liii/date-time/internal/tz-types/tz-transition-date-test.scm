;; tz-transition-date
;; 读取转移记录的日期规则字段。
;;
;; 语法
;; ----
;; tz-transition-date tr
;;
;; 参数
;; ----
;; tr : tz-transition? 转移记录。
;;
;; 返回值
;; ----
;; 转移日期规则记录。

(import (liii check) (liii date-time internal tz-types))

(check-set-mode! 'report-failed)

(define tr (tz-transition (tz-julian-without-leap-day 60) (tz-offset #f 2 0 0)))

(check-true (tz-julian-without-leap-day? (tz-transition-date tr)))
(check (tz-julian-without-leap-day:date (tz-transition-date tr)) => 60)

(check-report)
