(import (liii check)
        (liii date-time date))

(check-set-mode! 'report-failed)

;; date-iso-week
;; 获取日期所在的 ISO 周数（1-53）。
;;
;; 语法
;; ----
;; (date-iso-week date)
;;
;; 参数
;; ----
;; date : date? 日期对象。
;;
;; 返回值
;; ----
;; integer? 周数（1 到 53）。
;;
;; 示例
;; ----
;; (date-iso-week (make-date 2025 10 14)) => 42
;; (date-iso-week (make-date 2008 12 29)) => 1
;; (date-iso-week (make-date 2010 1 2)) => 53
;;
;; 错误处理
;; --------
;; type-error 当参数不是日期对象时抛出错误。

(check (date-iso-week (make-date 2025 10 14)) => 42)
(check (date-iso-week (make-date 2008 12 29)) => 1)
(check (date-iso-week (make-date 2010 1 2)) => 53)

(check-catch 'type-error (date-iso-week "2025-10-14"))
(check-catch 'type-error (date-iso-week 2025))

(check-report)
