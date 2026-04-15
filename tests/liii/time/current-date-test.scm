(import (liii check)
  (liii time)
  (srfi srfi-19)
) ;import


(check-set-mode! 'report-failed)


;; current-date
;; 获取当前日期对象。
;;
;; 语法
;; ----
;; (current-date [tz-offset])
;;
;; 参数
;; ----
;; tz-offset : integer? (可选) 时区偏移（秒），默认应为本地时区。
;;
;; 返回值
;; ----
;; date? 当前日期对象。
;;
;; 说明
;; ----
;; 1. 当前实现默认使用 UTC（tz-offset=0）。
;; 2. 规范要求默认使用本地时区，后续需要补接口支持。
;;
;; 错误处理
;; --------
;; wrong-type-arg 当 tz-offset 不是整数时抛出错误。


;; Test that current date can be converted
(check-true (date? (current-date 0)))
(check-true (string? (date->string (current-date 0))
            ) ;string?
) ;check-true
(check-true (string? (date->string (current-date 0)
                       "~Y年~m月~d日 ~H时~M分~S秒"
                     ) ;date->string
            ) ;string?
) ;check-true


;; current-date default tz-offset (local)
(let* ((offset (local-tz-offset))
       (d (current-date))
       (d2 (current-date offset))
      ) ;
  (check-true (date? d))
  (check (date-zone-offset d) => offset)
  (check (date-year d) => (date-year d2))
  (check (date-month d)
    =>
    (date-month d2)
  ) ;check
  (check (date-day d) => (date-day d2))
  (check (date-hour d) => (date-hour d2))
  (check (date-minute d)
    =>
    (date-minute d2)
  ) ;check
  (check (date-second d)
    =>
    (date-second d2)
  ) ;check
) ;let*


(check-report)
