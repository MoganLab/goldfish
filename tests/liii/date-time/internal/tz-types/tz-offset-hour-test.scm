;; tz-offset-hour
;; 读取偏移记录的小时字段。
;;
;; 语法
;; ----
;; tz-offset-hour off
;;
;; 参数
;; ----
;; off : tz-offset? 偏移记录。
;;
;; 返回值
;; ----
;; integer? 偏移小时数。

(import (liii check) (liii date-time internal tz-types))

(check-set-mode! 'report-failed)

(define off (tz-offset #f 5 30 0))

(check (tz-offset-hour off) => 5)

(check-report)
