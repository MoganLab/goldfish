(import (liii check)
        (liii date-time date))

(check-set-mode! 'report-failed)

;; date-iso-week-year
;; 获取日期所属的 ISO 周对应的年份（ISO week-numbering year）。
;;
;; 语法
;; ----
;; (date-iso-week-year date)
;;
;; 参数
;; ----
;; date : date? 日期对象。
;;
;; 返回值
;; ----
;; integer? ISO 周所属的年份。
;;
;; 示例
;; ----
;; (date-iso-week-year (make-date 2025 10 14)) => 2025
;; (date-iso-week-year (make-date 2008 12 29)) => 2009
;; (date-iso-week-year (make-date 2010 1 2)) => 2009
;;
;; 错误处理
;; --------
;; type-error 当参数不是日期对象时抛出错误。

(check (date-iso-week-year (make-date 2025 10 14)) => 2025)
(check (date-iso-week-year (make-date 2008 12 29)) => 2009)
(check (date-iso-week-year (make-date 2010 1 2)) => 2009)

(check-catch 'type-error (date-iso-week-year "2025-10-14"))
(check-catch 'type-error (date-iso-week-year 2025))

(check-report)
