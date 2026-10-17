(import (liii check)
        (liii date-time time-delta))

(check-set-mode! 'report-failed)

;; time-delta?
;; 判断给定对象是否为 time-delta 对象。
;;
;; 语法
;; ----
;; (time-delta? obj)
;;
;; 参数
;; ----
;; obj : any
;;
;; 返回值
;; ----
;; boolean? 若为 time-delta 返回 #t，否则返回 #f。
;;
;; 示例
;; ----
;; (time-delta? (years-delta 1)) => #t

(check-true (time-delta? (years-delta 1)))
(check-true (time-delta? (make-time-delta 0 0 0 0 0 0 0)))
(check-false (time-delta? 100))
(check-false (time-delta? #f))
(check-false (time-delta? "time-delta"))

(check-report)
