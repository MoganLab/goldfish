(import (liii check)
        (liii date-time time-delta)
        (liii date-time timezone)
        (liii date-time timestamp))

(check-set-mode! 'report-failed)

;; timestamp-
;; 计算时间戳减去时间增量。
;;
;; 语法
;; ----
;; (timestamp- ts dt)
;;
;; 参数
;; ----
;; ts : timestamp?
;; dt : time-delta?
;;
;; 返回值
;; ----
;; timestamp?
;;
;; 示例
;; ----
;; (timestamp- (make-timestamp 2021 1 1 1 0 0 (utc-timezone)) (hours-delta 1))

(check-true (timestamp=? (make-timestamp 2021 1 1 0 0 0 (utc-timezone))
                         (timestamp- (make-timestamp 2021 1 1 1 0 0 (utc-timezone)) (hours-delta 1))))

(check-catch 'type-error (timestamp- "invalid" (hours-delta 1)))

(check-report)
