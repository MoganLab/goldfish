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

;; 1 月初属于上一年 ISO 周年的边界日期
(check (date-iso-week-year (make-date 2016 1 1)) => 2015)
(check (date-iso-week-year (make-date 2022 1 1)) => 2021)
(check (date-iso-week-year (make-date 2022 1 2)) => 2021)
(check (date-iso-week-year (make-date 2022 1 3)) => 2022)
(check (date-iso-week-year (make-date 2023 1 1)) => 2022)
(check (date-iso-week-year (make-date 2023 1 2)) => 2023)
(check (date-iso-week-year (make-date 1993 1 3)) => 1992)
(check (date-iso-week-year (make-date 1994 1 1)) => 1993)
(check (date-iso-week-year (make-date 2017 1 1)) => 2016)

;; 12 月末属于下一年 ISO 周年的边界日期
(check (date-iso-week-year (make-date 2016 12 31)) => 2016)
(check (date-iso-week-year (make-date 2024 12 30)) => 2025)
(check (date-iso-week-year (make-date 2020 12 31)) => 2020)

(check-catch 'type-error (date-iso-week-year "2025-10-14"))
(check-catch 'type-error (date-iso-week-year 2025))

(check-report)
