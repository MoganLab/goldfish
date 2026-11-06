;; tz-offset-minute
;; 读取偏移记录的分钟字段。
;;
;; 语法
;; ----
;; tz-offset-minute off
;;
;; 参数
;; ----
;; off : tz-offset? 偏移记录。
;;
;; 返回值
;; ----
;; integer? 偏移分钟数。

(import (liii check) (liii date-time internal tz-types))

(check-set-mode! 'report-failed)

(define off (tz-offset #f 5 30 0))

(check (tz-offset-minute off) => 30)

(check-report)
