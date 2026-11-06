(import (liii check)
        (liii date-time gregorian))

(check-set-mode! 'report-failed)

;; last-in-month
;; 获取某年某月中最后一个指定的星期几对应的日期号（day of month）。
;;
;; 语法
;; ----
;; (last-in-month year month weekday)
;;
;; 参数
;; ----
;; year : integer? 年份。
;; month : integer? 月份（1-12）。
;; weekday : integer? ISO 星期几（1-7）。
;;
;; 返回值
;; ----
;; integer? 当月中对应的日。
;;
;; 示例
;; ----
;; (last-in-month 2025 10 5) => 31
;; (last-in-month 2025 10 1) => 27
;;
;; 错误处理
;; --------
;; type-error 当参数不是合法整数或超出范围时抛出错误。

;; 2025年10月有 31 天，31日是周五 (5)
;; 最后一个周五 (5) 是 10月31日
(check (last-in-month 2025 10 5) => 31)
;; 最后一个周一 (1) 是 10月27日
(check (last-in-month 2025 10 1) => 27)
;; 最后一个周日 (7) 是 10月26日
(check (last-in-month 2025 10 7) => 26)

(check-catch 'type-error (last-in-month 2025 10 0))
(check-catch 'type-error (last-in-month 2025 10 8))
(check-catch 'type-error (last-in-month 2025 0 1))
(check-catch 'type-error (last-in-month 2025 10 'foo))

(check-report)
