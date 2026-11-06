;; tz-transition-time
;; 读取转移记录的时刻字段。
;;
;; 语法
;; ----
;; tz-transition-time tr
;;
;; 参数
;; ----
;; tr : tz-transition? 转移记录。
;;
;; 返回值
;; ----
;; tz-offset? 转移发生的本地时刻。

(import (liii check) (liii date-time internal tz-types))

(check-set-mode! 'report-failed)

(define tr (tz-transition (tz-julian-without-leap-day 60) (tz-offset #f 2 0 0)))

(check (tz-offset-hour (tz-transition-time tr)) => 2)

(check-report)
