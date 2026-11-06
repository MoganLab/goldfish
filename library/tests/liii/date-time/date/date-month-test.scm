(import (liii check)
        (liii date-time date))

(check-set-mode! 'report-failed)

;; date-month
;; 获取日期对象的月份。
;;
;; 语法
;; ----
;; (date-month date)
;;
;; 参数
;; ----
;; date : date? 日期对象。
;;
;; 返回值
;; ----
;; integer? 月份（1-12）。
;;
;; 示例
;; ----
;; (date-month (make-date 2000 1 2)) => 1
;;
;; 错误处理
;; --------
;; type-error 当参数不是日期对象时抛出错误。

(check (date-month (make-date 2000 1 2)) => 1)
(check (date-month (make-date 2020 12 31)) => 12)

(check-catch 'type-error (date-month "2000-01-02"))
(check-catch 'type-error (date-month 1))
(check-catch 'type-error (date-month 'foo))

(check-report)
