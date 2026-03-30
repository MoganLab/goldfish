(import (liii check)
        (liii time)
        (srfi srfi-19)
) ;import

(check-set-mode! 'report-failed)

;; date->string string->date
;; 日期和字符串之间的转换函数。
;;
;; 语法
;; ----
;; (date->string date [format-string])
;; (string->date input-string template-string)
;;
;; 参数
;; ----
;; date : date? 要转换的日期对象。
;; format-string : string? (可选) 格式字符串，默认为 "~c"。
;; input-string : string? 需要解析的输入字符串。
;; template-string : string? 模板字符串。
;;
;; 返回值
;; ----
;; date->string : string? 表示日期的字符串。
;; string->date : date? 解析得到的日期对象。
;;
;; 错误处理
;; --------
;; wrong-type-arg 当参数类型不正确时抛出错误。
;; value-error 当输入字符串无法匹配模板时抛出错误。

;; Test date->string
(let ((d (make-date 0 0 0 0 1 1 1970 0)))  ; Unix epoch in UTC
  (check (date->string d)            => "Thu Jan 01 00:00:00Z 1970")
  (check (date->string d "~Y-~m-~d") => "1970-01-01")
  (check (date->string d "~H:~M:~S") => "00:00:00")
) ;let

;; Test with different dates
(let ((d1 (make-date 500000000 30 15 9 4 7 1776 0))          ; US Independence
      (d2 (make-date 123456789 45 30 14 25 12 2023 28800))   ; Christmas in UTC+8
      (d3 (make-date 999999999 59 59 23 31 12 1999 -18000))) ; Y2K in UTC-5
  (check (date->string d1)                             => "Thu Jul 04 09:15:30Z 1776")
  (check (date->string d2 "~Y-~m-~d ~H:~M:~S")         => "2023-12-25 14:30:45")
  (check (date->string d3 "~A, ~B ~d, ~Y ~I:~M:~S ~p") => "Friday, December 31, 1999 11:59:59 PM")
) ;let

;; Roundtrip date->string -> string->date
(let* ((d (make-date 0 45 30 14 25 12 2023 28800))
       (fmt "~Y-~m-~d ~H:~M:~S~z")
       (s (date->string d fmt))
       (d2 (string->date s fmt)))
  (check (date-year d2) => 2023)
  (check (date-month d2) => 12)
  (check (date-day d2) => 25)
  (check (date-hour d2) => 14)
  (check (date-minute d2) => 30)
  (check (date-second d2) => 45)
  (check (date-zone-offset d2) => 28800)
  (check (date->string d2 fmt) => s)
) ;let*

;; Test string->date
(let* ((s "2023-12-25 14:30:45")
       (d (string->date s "~Y-~m-~d ~H:~M:~S")))
  (check (date-year d) => 2023)
  (check (date-month d) => 12)
  (check (date-day d) => 25)
  (check (date-hour d) => 14)
  (check (date-minute d) => 30)
  (check (date-second d) => 45)
) ;let*

;; Test error conditions
(let ((d (make-date 0 0 0 0 1 1 1970 0)))
  (check-catch 'wrong-type-arg (date->string "not-a-date"))
  (check-catch 'wrong-type-arg (date->string d 123))
  (check-catch 'wrong-type-arg (date->string d 'symbol))
) ;let

(check-catch 'wrong-type-arg (string->date 1 "~Y"))
(check-catch 'wrong-type-arg (string->date "2020" 123))
(check-catch 'value-error (string->date "2020-01-01" "~Y/~m/~d"))

(check-report)
