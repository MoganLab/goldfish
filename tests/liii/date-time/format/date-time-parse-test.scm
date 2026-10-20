(import (liii check)
        (liii date-time date)
        (liii date-time clock-time)
        (liii date-time timezone)
        (liii date-time timestamp)
        (liii date-time format))

(check-set-mode! 'report-failed)

;; date-time-parse
;; 将格式化的字符串解析为 timestamp 对象，说明符与 Python datetime.strptime 对齐。
;;
;; 语法
;; ----
;; (date-time-parse str [format-string] [default-tz])
;;
;; 参数
;; ----
;; str : string? 输入字符串
;; format-string : string? 可选，默认 "%Y-%m-%d %H:%M:%S"
;; default-tz : timezone? 可选，当输入字符串未包含时区时使用，默认 (system-timezone)
;;
;; 返回值
;; ----
;; timestamp?
;;
;; 示例
;; ----
;; (date-time-parse "2024-05-09 14:30:05" "%Y-%m-%d %H:%M:%S" (utc-timezone))
;;
;; 错误处理
;; --------
;; type-error 当参数类型不匹配时抛出错误
;; value-error 当输入字符串与格式不匹配、数字非法时抛出错误

(define utc (utc-timezone))

;; 1. 基础解析与默认时区
(let ((ts (date-time-parse "2024-05-09 14:30:05" "%Y-%m-%d %H:%M:%S" utc)))
  (check (timestamp-year ts) => 2024)
  (check (timestamp-month ts) => 5)
  (check (timestamp-day ts) => 9)
  (check (timestamp-hour ts) => 14)
  (check (timestamp-minute ts) => 30)
  (check (timestamp-second ts) => 5))

;; 2. 缺省格式与缺省时区
(let ((ts (date-time-parse "2024-05-09 14:30:05")))
  (check (timestamp-year ts) => 2024)
  (check (timestamp-month ts) => 5)
  (check (timestamp-day ts) => 9)
  (check (timestamp-hour ts) => 14)
  (check (timestamp-minute ts) => 30)
  (check (timestamp-second ts) => 5))

;; 3. 仅日期格式（时分秒默认为 00:00:00）
(let ((ts (date-time-parse "2024-05-09" "%Y-%m-%d" utc)))
  (check (timestamp-year ts) => 2024)
  (check (timestamp-month ts) => 5)
  (check (timestamp-day ts) => 9)
  (check (timestamp-hour ts) => 0)
  (check (timestamp-minute ts) => 0)
  (check (timestamp-second ts) => 0))

;; 4. 快捷格式 %F, %T
(let ((ts (date-time-parse "2024-05-09 14:30:05" "%F %T" utc)))
  (check (timestamp-year ts) => 2024)
  (check (timestamp-second ts) => 5))

;; 5. 2 位年份回算（与 Python strptime 规范对齐: 00-68 对应 2000-2068, 69-99 对应 1969-1999）
(let ((ts-24 (date-time-parse "24-05-09 14:30:05" "%y-%m-%d %H:%M:%S" utc))
      (ts-68 (date-time-parse "68-01-01" "%y-%m-%d" utc))
      (ts-69 (date-time-parse "69-01-01" "%y-%m-%d" utc))
      (ts-99 (date-time-parse "99-12-31" "%y-%m-%d" utc)))
  (check (timestamp-year ts-24) => 2024)
  (check (timestamp-year ts-68) => 2068)
  (check (timestamp-year ts-69) => 1969)
  (check (timestamp-year ts-99) => 1999))

;; 6. 12 小时制与 %p（与 Python 对齐）
(let ((ts-pm (date-time-parse "2024-05-09 02:30:05 PM" "%Y-%m-%d %I:%M:%S %p" utc))
      (ts-am (date-time-parse "2024-05-09 12:30:05 AM" "%Y-%m-%d %I:%M:%S %p" utc)))
  (check (timestamp-hour ts-pm) => 14)
  (check (timestamp-hour ts-am) => 0))

;; 7. 微秒解析 %f（与 Python 对齐，6位或前缀匹配）
(let ((ts (date-time-parse "2024-05-09 14:30:05.123456" "%Y-%m-%d %H:%M:%S.%f" utc)))
  (check (timestamp-second ts) => (+ 5 123456/1000000)))

;; 8. 英文月份名称 (%b, %B)
(let ((ts-b (date-time-parse "09-May-2024" "%d-%b-%Y" utc))
      (ts-jan (date-time-parse "January 15, 2024" "%B %d, %Y" utc)))
  (check (timestamp-month ts-b) => 5)
  (check (timestamp-day ts-b) => 9)
  (check (timestamp-month ts-jan) => 1)
  (check (timestamp-day ts-jan) => 15))

;; 9. 年内天数解析 %j（与 Python 对齐）
(let ((ts-j (date-time-parse "2024 130" "%Y %j" utc)))
  (check (timestamp-year ts-j) => 2024)
  (check (timestamp-month ts-j) => 5)
  (check (timestamp-day ts-j) => 9))

;; 10. 时区解析 (%z 和 %:z 优先于 default-tz)
(let ((ts-z (date-time-parse "2024-05-09 14:30:05+0800" "%Y-%m-%d %H:%M:%S%z" utc))
      (ts-colon (date-time-parse "2024-05-09 14:30:05+08:00" "%Y-%m-%d %H:%M:%S%:z" utc))
      (ts-utc-z (date-time-parse "2024-05-09 14:30:05Z" "%Y-%m-%d %H:%M:%S%z")))
  (check (timestamp-year ts-z) => 2024)
  (check (timestamp-hour ts-z) => 14)
  (check (date-time-format ts-z "%z") => "+0800")
  (check (date-time-format ts-colon "%:z") => "+08:00")
  (check (date-time-format ts-utc-z "%z") => "+0000"))

;; 11. Roundtrip 往返测试 (format -> parse -> format)
(let* ((orig (make-timestamp 2024 10 25 8 9 10 utc))
       (fmt "%Y/%m/%d %H:%M:%S")
       (str (date-time-format orig fmt))
       (parsed (date-time-parse str fmt utc)))
  (check (date-time-format parsed fmt) => str)
  (check (timestamp=? orig parsed) => #t))

;; 12. 错误处理
(check-catch 'type-error (date-time-parse 123))
(check-catch 'type-error (date-time-parse "2024-05-09" 456))
(check-catch 'type-error (date-time-parse "2024-05-09" "%Y-%m-%d" "not-a-tz"))
(check-catch 'value-error (date-time-parse "2024-05-09" "%Y/%m/%d"))
(check-catch 'value-error (date-time-parse "2024-99-99" "%Y-%m-%d"))
(check-catch 'value-error (date-time-parse "abc" "%Y"))

(check-report)
