;; tz-offset-second
;; 读取偏移记录的秒字段。
;;
;; 语法
;; ----
;; tz-offset-second off
;;
;; 参数
;; ----
;; off : tz-offset? 偏移记录。
;;
;; 返回值
;; ----
;; integer? 偏移秒数。

(import (liii check) (liii date-time internal tz-types))

(check-set-mode! 'report-failed)

(define off (tz-offset #f 5 30 15))

(check (tz-offset-second off) => 15)

(check-report)
