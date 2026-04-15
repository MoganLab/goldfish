(import (liii check)
  (liii time)
  (srfi srfi-19)
) ;import


(check-set-mode! 'report-failed)


;; string->date
;; 将字符串解析为日期对象。
;;
;; 语法
;; ----
;; (string->date input-string template-string)
;;
;; 参数
;; ----
;; input-string : string? 需要解析的输入字符串。
;; template-string : string? 模板字符串。
;;
;; 返回值
;; ----
;; date? 解析得到的日期对象。
;;
;; 错误处理
;; --------
;; wrong-type-arg 当参数类型不正确时抛出错误。
;; value-error 当输入字符串无法匹配模板时抛出错误。


;; Test string->date basic
(let* ((s "2023-12-25 14:30:45")
       (d (string->date s "~Y-~m-~d ~H:~M:~S"))
      ) ;
  (check (date-year d) => 2023)
  (check (date-month d) => 12)
  (check (date-day d) => 25)
  (check (date-hour d) => 14)
  (check (date-minute d) => 30)
  (check (date-second d) => 45)
) ;let*


;; Roundtrip date->string -> string->date
(let* ((d (make-date 0 45 30 14 25 12 2023 28800)
       ) ;d
       (fmt "~Y-~m-~d ~H:~M:~S~z")
       (s (date->string d fmt))
       (d2 (string->date s fmt))
      ) ;
  (check (date-year d2) => 2023)
  (check (date-month d2) => 12)
  (check (date-day d2) => 25)
  (check (date-hour d2) => 14)
  (check (date-minute d2) => 30)
  (check (date-second d2) => 45)
  (check (date-zone-offset d2) => 28800)
  (check (date->string d2 fmt) => s)
) ;let*


;; Test error conditions
(check-catch 'wrong-type-arg
  (string->date 1 "~Y")
) ;check-catch
(check-catch 'wrong-type-arg
  (string->date "2020" 123)
) ;check-catch
(check-catch 'value-error
  (string->date "2020-01-01" "~Y/~m/~d")
) ;check-catch


(check-report)
