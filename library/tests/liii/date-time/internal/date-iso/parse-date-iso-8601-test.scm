;; parse-date-iso-8601
;; 解析 YYYY-MM-DD 字符串为日期对象。
;;
;; 语法
;; ----
;; (parse-date-iso-8601 str)
;;
;; 参数
;; ----
;; str : string? ISO-8601 日期字符串。
;;
;; 返回值
;; ----
;; date? 解析得到的日期对象。

(import (liii check) (liii date-time date) (liii date-time internal date-iso))

(check-set-mode! 'report-failed)

(check
  (let-values (((y m d) (date-ymd (parse-date-iso-8601 "2021-01-02"))))
    (list y m d)
  ) ;let-values
  =>
  '(2021 1 2)
) ;check
(check (date-year (parse-date-iso-8601 "-0044-03-15")) => -44)
(check-catch 'type-error (parse-date-iso-8601 "abc"))
(check-catch 'type-error (parse-date-iso-8601 "2021-1"))
(check-catch 'type-error (parse-date-iso-8601 "2021-13-01"))
(check-catch 'type-error (parse-date-iso-8601 20210102))

(check-report)
