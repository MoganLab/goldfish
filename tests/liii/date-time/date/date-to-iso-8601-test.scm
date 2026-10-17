(import (liii check)
        (liii date-time date))

(check-set-mode! 'report-failed)

;; date->iso-8601
;; 将日期对象转换为 ISO-8601 格式的字符串（YYYY-MM-DD）。
;;
;; 语法
;; ----
;; (date->iso-8601 date)
;;
;; 参数
;; ----
;; date : date? 日期对象。
;;
;; 返回值
;; ----
;; string? 形如 "2020-01-01" 的字符串。
;;
;; 示例
;; ----
;; (date->iso-8601 (make-date 2020 1 1)) => "2020-01-01"
;;
;; 错误处理
;; --------
;; type-error 当参数不是日期对象时抛出错误。

(check (date->iso-8601 (make-date 2020 1 1)) => "2020-01-01")
(check (date->iso-8601 (make-date 2020 11 12)) => "2020-11-12")
(check (date->iso-8601 (make-date 0 1 1)) => "0000-01-01")
(check (date->iso-8601 (make-date -1 1 1)) => "-0001-01-01")
(check (date->iso-8601 (make-date 99999 1 1)) => "99999-01-01")

(check-catch 'type-error (date->iso-8601 "2020-01-01"))
(check-catch 'type-error (date->iso-8601 20200101))

(check-report)
