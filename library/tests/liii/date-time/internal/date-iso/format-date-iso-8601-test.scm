;; format-date-iso-8601
;; 将日期格式化为 YYYY-MM-DD 字符串。
;;
;; 语法
;; ----
;; (format-date-iso-8601 date)
;;
;; 参数
;; ----
;; date : date? 日期对象。
;;
;; 返回值
;; ----
;; string? ISO-8601 日期字符串，负年份带 '-' 前缀。

(import (liii check) (liii date-time date) (liii date-time internal date-iso))

(check-set-mode! 'report-failed)

(check (format-date-iso-8601 (make-date 2021 1 2)) => "2021-01-02")
(check (format-date-iso-8601 (make-date 5 1 1)) => "0005-01-01")
(check (format-date-iso-8601 (make-date -44 3 15)) => "-0044-03-15")
(check-catch 'type-error (format-date-iso-8601 "date"))

(check-report)
