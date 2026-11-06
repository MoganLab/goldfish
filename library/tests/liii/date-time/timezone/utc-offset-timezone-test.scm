(import (liii check)
        (liii date-time timezone)
        (liii date-time time-delta))

(check-set-mode! 'report-failed)

;; utc-offset-timezone
;; 根据固定时间偏移量创建时区对象。
;;
;; 语法
;; ----
;; (utc-offset-timezone offset)
;;
;; 参数
;; ----
;; offset : time-delta? 时间偏移量（仅允许包含小时、分钟、秒分量）。
;;
;; 返回值
;; ----
;; timezone? 固定偏移的时区对象。
;;
;; 示例
;; ----
;; (utc-offset-timezone (hours-delta 8))
;;
;; 错误处理
;; --------
;; type-error 当参数不是合法时间增量或包含年月日周时抛出错误。

(check-true (timezone? (utc-offset-timezone (hours-delta 8))))
(check-true (timezone? (utc-offset-timezone (hours-delta -5))))
(check-true (timezone? (utc-offset-timezone (time-delta+ (hours-delta 5) (minutes-delta 30)))))

(check-catch 'type-error (utc-offset-timezone (days-delta 1)))
(check-catch 'type-error (utc-offset-timezone 8))

(check-report)
