(import (liii check)
        (liii date-time gregorian))

(check-set-mode! 'report-failed)

;; nth-in-month
;; 获取某年某月中第 n 个指定的星期几对应的日期号（day of month）。
;; 若超出该月所包含的该星期的总次数，返回 #f。
;;
;; 语法
;; ----
;; (nth-in-month year month weekday n)
;;
;; 参数
;; ----
;; year : integer? 年份。
;; month : integer? 月份（1-12）。
;; weekday : integer? ISO 星期几（1-7）。
;; n : integer? 第几个（>= 1）。
;;
;; 返回值
;; ----
;; integer? 或 #f。
;;
;; 示例
;; ----
;; (nth-in-month 2025 10 1 1) => 6
;; (nth-in-month 2025 10 1 5) => #f
;;
;; 错误处理
;; --------
;; type-error 当参数不是合法整数或超出范围时抛出错误。

;; 2025年10月：
;; 周一是 6, 13, 20, 27
(check (nth-in-month 2025 10 1 1) => 6)
(check (nth-in-month 2025 10 1 2) => 13)
(check (nth-in-month 2025 10 1 3) => 20)
(check (nth-in-month 2025 10 1 4) => 27)
(check (nth-in-month 2025 10 1 5) => #f)

(check-catch 'type-error (nth-in-month 2025 10 1 0))
(check-catch 'type-error (nth-in-month 2025 10 1 -1))
(check-catch 'type-error (nth-in-month 2025 10 8 1))
(check-catch 'type-error (nth-in-month "2025" 10 1 1))

(check-report)
