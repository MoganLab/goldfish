(import (liii check)
        (liii date-time gregorian))

(check-set-mode! 'report-failed)

;; first-in-month
;; 获取某年某月中第一个指定的星期几对应的日期号（day of month）。
;;
;; 语法
;; ----
;; (first-in-month year month weekday)
;;
;; 参数
;; ----
;; year : integer? 年份。
;; month : integer? 月份（1-12）。
;; weekday : integer? ISO 星期几（1-7）。
;;
;; 返回值
;; ----
;; integer? 当月中对应的日（1-7）。
;;
;; 示例
;; ----
;; (first-in-month 2025 10 1) => 6
;; (first-in-month 2025 10 3) => 1
;;
;; 错误处理
;; --------
;; type-error 当参数不是合法整数或超出范围时抛出错误。

;; 2025年10月1日是周三 (3)
;; 第一个周一 (1) 是 10月6日
(check (first-in-month 2025 10 1) => 6)
;; 第一个周三 (3) 是 10月1日
(check (first-in-month 2025 10 3) => 1)
;; 第一个周日 (7) 是 10月5日
(check (first-in-month 2025 10 7) => 5)

(check-catch 'type-error (first-in-month 2025 10 0))
(check-catch 'type-error (first-in-month 2025 10 8))
(check-catch 'type-error (first-in-month 2025 13 1))
(check-catch 'type-error (first-in-month "2025" 10 1))

(check-report)
