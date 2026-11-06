;; time?
;; 判断字符串是否为 RFC 3339 完整时间。
;;
;; 语法
;; ----
;; (time? value)
;;
;; 参数
;; ----
;; value : string
;; 待检查的字符串。
;;
;; 返回值
;; ----
;; boolean
;; 是合法的完整时间返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; 格式为 HH:MM:SS[.小数]，并且必须带时区：
;; Z、z 或 ±HH:MM 偏移。
;; 闰秒（秒数为 60）只有在对应 UTC 时刻为 23:59 时才被接受。
;;
;; 错误处理
;; ----
;; 无。

(import (liii check) (liii json schema format-datetime))

(check-set-mode! 'report-failed)

;; 合法时间。

(check-true (time? "12:00:00Z"))
(check-true (time? "12:00:00z"))
(check-true (time? "23:59:59Z"))
(check-true (time? "00:00:00Z"))
(check-true (time? "12:00:00+01:00"))
(check-true (time? "12:00:00-05:30"))
(check-true (time? "12:00:00.123Z"))
(check-true (time? "23:20:50.52Z"))

;; 闰秒只在 UTC 23:59 时合法。

(check-true (time? "23:59:60Z"))
(check-true (time? "23:59:60+00:00"))
(check-true (time? "01:29:60+01:30"))
(check-true (time? "15:59:60-08:00"))
(check-false (time? "22:59:60Z"))
(check-false (time? "23:58:60Z"))
(check-false (time? "23:59:60+01:00"))
(check-false (time? "23:59:60-01:00"))

;; 缺少时区不被接受。

(check-false (time? "12:00:00"))
(check-false (time? "12:00:00.123"))

;; 时分秒越界。

(check-false (time? "24:00:00Z"))
(check-false (time? "12:60:00Z"))
(check-false (time? "12:00:61Z"))

;; 时区偏移越界。

(check-false (time? "12:00:00+24:00"))
(check-false (time? "12:00:00+00:60"))

;; 格式错误。

(check-false (time? "12:00Z"))
(check-false (time? "12:00:00.Z"))

(check-report)
