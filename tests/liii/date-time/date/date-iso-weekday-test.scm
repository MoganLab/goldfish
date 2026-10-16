(import (liii check)
        (liii date-time date))

(check-set-mode! 'report-failed)

;; date-iso-weekday
;; 获取日期对象的 ISO 星期几（1-7，周一为 1，周日为 7）。
;;
;; 语法
;; ----
;; (date-iso-weekday date)
;;
;; 参数
;; ----
;; date : date? 日期对象。
;;
;; 返回值
;; ----
;; integer? 1 到 7 的整数。
;;
;; 示例
;; ----
;; (date-iso-weekday (make-date 2025 10 12)) => 7
;; (date-iso-weekday (make-date 2025 10 13)) => 1
;;
;; 错误处理
;; --------
;; type-error 当参数不是日期对象时抛出错误。

(check (date-iso-weekday (make-date 2025 10 12)) => 7)
(check (date-iso-weekday (make-date 2025 10 13)) => 1)
(check (date-iso-weekday (make-date 2025 10 14)) => 2)
(check (date-iso-weekday (make-date 2025 10 15)) => 3)
(check (date-iso-weekday (make-date 2025 10 16)) => 4)
(check (date-iso-weekday (make-date 2025 10 17)) => 5)
(check (date-iso-weekday (make-date 2025 10 18)) => 6)

(check-catch 'type-error (date-iso-weekday "2025-10-12"))
(check-catch 'type-error (date-iso-weekday 123))

(check-report)
