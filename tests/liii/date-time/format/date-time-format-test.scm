(import (liii check)
        (liii date-time date)
        (liii date-time clock-time)
        (liii date-time moment)
        (liii date-time timezone)
        (liii date-time timestamp)
        (liii date-time format))

(check-set-mode! 'report-failed)

;; date-time-format
;; 将日期时间对象按格式字符串格式化为字符串，格式说明符与 Python datetime.strftime 对齐。
;;
;; 语法
;; ----
;; (date-time-format obj [format-string])
;;
;; 参数
;; ----
;; obj : (or/c timestamp? date? clock-time? moment?)
;; format-string : string? 可选，默认 "%Y-%m-%d %H:%M:%S"
;;
;; 返回值
;; ----
;; string
;;
;; 示例
;; ----
;; (date-time-format (make-timestamp 2024 5 9 14 30 5 (utc-timezone)))
;; => "2024-05-09 14:30:05"
;;
;; 错误处理
;; --------
;; type-error 当参数类型不匹配时抛出错误
;; value-error 当格式字符串未闭合或遇到非法转义符时抛出错误

(define ts (make-timestamp 2024 5 9 14 30 5 (utc-timezone)))

;; 1. 默认格式 (ISO 格式)
(check (date-time-format ts) => "2024-05-09 14:30:05")

;; 2. 年月日说明符（与 Python 对齐）
(check (date-time-format ts "%Y") => "2024")
(check (date-time-format ts "%y") => "24")
(check (date-time-format ts "%m") => "05")
(check (date-time-format ts "%d") => "09")
(check (date-time-format ts "%j") => "130") ; 年内第130天 (001-366)
(check (date-time-format ts "%F") => "2024-05-09")

;; 3. 时分秒与精度说明符（与 Python 对齐）
(check (date-time-format ts "%H") => "14")
(check (date-time-format ts "%I") => "02")
(check (date-time-format ts "%M") => "30")
(check (date-time-format ts "%S") => "05")
(check (date-time-format ts "%p") => "PM")
(check (date-time-format ts "%T") => "14:30:05")
(check (date-time-format ts "%R") => "14:30")

;; 4. 12 小时制与 %p（与 Python 对齐）
(let ((ts-am (make-timestamp 2024 1 1 0 15 30 (utc-timezone))))
  (check (date-time-format ts-am "%I %p") => "12 AM"))

(let ((ts-noon (make-timestamp 2024 1 1 12 0 0 (utc-timezone))))
  (check (date-time-format ts-noon "%I %p") => "12 PM"))

;; 5. 微秒说明符 %f（与 Python 对齐，6位补零）
(let ((ts-frac (make-timestamp 2024 5 9 14 30 (+ 5 123456/1000000) (utc-timezone))))
  (check (date-time-format ts-frac "%S.%f") => "05.123456"))

(check (date-time-format ts "%f") => "000000")

;; 6. 星期与月份名称（与 Python 对齐）
;; 2024-05-09 是周四 Thursday
(check (date-time-format ts "%a") => "Thu")
(check (date-time-format ts "%A") => "Thursday")
(check (date-time-format ts "%b") => "May")
(check (date-time-format ts "%B") => "May")
(check (date-time-format ts "%w") => "4") ; Python 规范: 0 为周日，4 为周四

;; 周日测试: 2024-01-07 是周日 Sunday (%w 应为 0)
(let ((ts-sun (make-timestamp 2024 1 7 10 0 0 (utc-timezone))))
  (check (date-time-format ts-sun "%a") => "Sun")
  (check (date-time-format ts-sun "%A") => "Sunday")
  (check (date-time-format ts-sun "%w") => "0")
  (check (date-time-format ts-sun "%b") => "Jan")
  (check (date-time-format ts-sun "%B") => "January")
  (check (date-time-format ts-sun "%j") => "007"))

;; 7. 时区说明符（与 Python 对齐）
(check (date-time-format ts "%z") => "+0000")
(check (date-time-format ts "%:z") => "+00:00")
(check (date-time-format ts "%Z") => "UTC")

(let ((ts-sh (make-timestamp 2024 5 9 14 30 5 (tz-timezone "Asia/Shanghai"))))
  (check (date-time-format ts-sh "%z") => "+0800")
  (check (date-time-format ts-sh "%:z") => "+08:00")
  (check (date-time-format ts-sh "%Z") => "CST"))

;; 8. 复合与快捷说明符（与 Python 对齐）
(check (date-time-format ts "%x") => "05/09/24")
(check (date-time-format ts "%X") => "14:30:05")
(check (date-time-format ts "%c") => "Thu May  9 14:30:05 2024")

;; 9. 转义与字面量
(check (date-time-format ts "%%") => "%")
(check (date-time-format ts "%Y%%%m") => "2024%05")
(check (date-time-format ts "%Y-%m-%d %H:%M:%S") => "2024-05-09 14:30:05")

;; 10. 多态支持
(let ((d (make-date 2024 5 9)))
  (check (date-time-format d "%F") => "2024-05-09")
  (check (date-time-format d "%Y/%m/%d") => "2024/05/09")
  (check (date-time-format d "%A") => "Thursday")
  (check (date-time-format d "%w") => "4"))

(let ((ct (make-clock-time 14 30 5)))
  (check (date-time-format ct "%T") => "14:30:05")
  (check (date-time-format ct "%H:%M") => "14:30"))

(let ((m (make-moment (make-date 2024 5 9) (+ (* 14 3600) (* 30 60) 5))))
  (check (date-time-format m "%F %T") => "2024-05-09 14:30:05"))

;; 11. 错误处理
(check-catch 'type-error (date-time-format 12345))
(check-catch 'type-error (date-time-format "not-a-date"))
(check-catch 'type-error (date-time-format ts 123))
(check-catch 'value-error (date-time-format ts "%"))
(check-catch 'value-error (date-time-format ts "%Y-%m-%d %Q"))

(check-report)
