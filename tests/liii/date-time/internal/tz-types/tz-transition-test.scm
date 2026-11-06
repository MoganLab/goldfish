;; tz-transition
;; 构造 POSIX 转移日期与时刻记录。
;;
;; 语法
;; ----
;; tz-transition date time
;;
;; 参数
;; ----
;; date : 转移日期规则记录。
;; time : tz-offset? 转移发生的本地时刻。
;;
;; 返回值
;; ----
;; tz-transition? 新建的转移记录。

(import (liii check) (liii date-time internal tz-types))

(check-set-mode! 'report-failed)

(define tr (tz-transition (tz-julian-without-leap-day 60) (tz-offset #f 2 0 0)))

(check-true (tz-transition? tr))

(check-report)
