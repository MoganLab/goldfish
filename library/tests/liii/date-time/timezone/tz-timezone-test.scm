(import (liii check)
        (liii date-time timezone))

(check-set-mode! 'report-failed)

;; tz-timezone
;; 根据 IANA 时区名称（如 "Europe/Vilnius", "America/New_York", "Asia/Shanghai"）获取时区对象。
;;
;; 语法
;; ----
;; (tz-timezone name)
;;
;; 参数
;; ----
;; name : string? IANA 时区标识名。
;;
;; 返回值
;; ----
;; timezone? 时区对象。
;;
;; 示例
;; ----
;; (tz-timezone "Europe/Vilnius")
;;
;; 错误处理
;; --------
;; type-error 当参数不是字符串或时区不存在时抛出错误。

(check-true (timezone? (tz-timezone "Europe/Vilnius")))
(check-true (timezone? (tz-timezone "America/New_York")))
(check-true (timezone? (tz-timezone "UTC")))

(check-catch 'type-error (tz-timezone 123))
(check-catch 'type-error (tz-timezone "NonExistent/Timezone"))

(check-report)
