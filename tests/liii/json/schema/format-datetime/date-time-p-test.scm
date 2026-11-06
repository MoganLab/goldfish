;; date-time?
;; 判断字符串是否为 RFC 3339 日期时间。
;;
;; 语法
;; ----
;; (date-time? value)
;;
;; 参数
;; ----
;; value : string
;; 待检查的字符串。
;;
;; 返回值
;; ----
;; boolean
;; 是合法的日期时间返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; 格式为 <日期>T<时间>，日期与时间之间使用大写 T 或小写 t 分隔，
;; 时间部分必须带时区。
;;
;; 错误处理
;; ----
;; 无。

(import (liii check) (liii json schema format-datetime))

(check-set-mode! 'report-failed)

;; 合法日期时间。

(check-true (date-time? "2020-01-01T12:00:00Z"))
(check-true (date-time? "2020-01-01t12:00:00Z"))
(check-true (date-time? "2020-12-31T23:59:59+01:00"))
(check-true (date-time? "2020-02-29T00:00:00.123Z"))

;; 分隔符必须是 T 或 t。

(check-false (date-time? "2020-01-01 12:00:00Z"))

;; 时间部分缺少时区。

(check-false (date-time? "2020-01-01T12:00:00"))

;; 日期或时间本身不合法。

(check-false (date-time? "2020-13-01T12:00:00Z"))
(check-false (date-time? "2020-01-01T25:00:00Z"))

(check-report)
