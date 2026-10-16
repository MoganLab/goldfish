(import (liii check)
        (liii date-time date))

(check-set-mode! 'report-failed)

;; date-day
;; 获取日期对象的日。
;;
;; 语法
;; ----
;; (date-day date)
;;
;; 参数
;; ----
;; date : date? 日期对象。
;;
;; 返回值
;; ----
;; integer? 日（1-31）。
;;
;; 示例
;; ----
;; (date-day (make-date 2000 1 2)) => 2
;;
;; 错误处理
;; --------
;; type-error 当参数不是日期对象时抛出错误。

(check (date-day (make-date 2000 1 2)) => 2)
(check (date-day (make-date 2020 2 29)) => 29)

(check-catch 'type-error (date-day "2000-01-02"))
(check-catch 'type-error (date-day 2))
(check-catch 'type-error (date-day #f))

(check-report)
