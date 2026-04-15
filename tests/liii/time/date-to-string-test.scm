(import (liii check)
  (liii time)
  (srfi srfi-19)
) ;import


(check-set-mode! 'report-failed)


;; date->string
;; 将日期对象转换为字符串。
;;
;; 语法
;; ----
;; (date->string date [format-string])
;;
;; 参数
;; ----
;; date : date? 要转换的日期对象。
;; format-string : string? (可选) 格式字符串，默认为 "~c"。
;;
;; 返回值
;; ----
;; string? 表示日期的字符串。
;;
;; 错误处理
;; --------
;; wrong-type-arg 当参数类型不正确时抛出错误。


;; Test date->string basic
(let ((d (make-date 0 0 0 0 1 1 1970 0)))
  (check (date->string d)
    =>
    "Thu Jan 01 00:00:00Z 1970"
  ) ;check
  (check (date->string d "~Y-~m-~d")
    =>
    "1970-01-01"
  ) ;check
  (check (date->string d "~H:~M:~S")
    =>
    "00:00:00"
  ) ;check
) ;let


;; Test with different dates
(let ((d1 (make-date 500000000 30 15 9 4 7 1776 0)
      ) ;d1
      (d2 (make-date 123456789
            45
            30
            14
            25
            12
            2023
            28800
          ) ;make-date
      ) ;d2
      (d3 (make-date 999999999
            59
            59
            23
            31
            12
            1999
            -18000
          ) ;make-date
      ) ;d3
     ) ;
  (check (date->string d1)
    =>
    "Thu Jul 04 09:15:30Z 1776"
  ) ;check
  (check (date->string d2 "~Y-~m-~d ~H:~M:~S")
    =>
    "2023-12-25 14:30:45"
  ) ;check
  (check (date->string d3
           "~A, ~B ~d, ~Y ~I:~M:~S ~p"
         ) ;date->string
    =>
    "Friday, December 31, 1999 11:59:59 PM"
  ) ;check
) ;let


;; Test error conditions
(let ((d (make-date 0 0 0 0 1 1 1970 0)))
  (check-catch 'wrong-type-arg
    (date->string "not-a-date")
  ) ;check-catch
  (check-catch 'wrong-type-arg
    (date->string d 123)
  ) ;check-catch
  (check-catch 'wrong-type-arg
    (date->string d 'symbol)
  ) ;check-catch
) ;let


(check-report)
