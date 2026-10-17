(import (liii check)
        (liii date-time tzdb))

(check-set-mode! 'report-failed)

;; tz-timezones
;; 获取系统所有可用的 IANA 时区名称列表。
;;
;; 语法
;; ----
;; (tz-timezones)
;;
;; 返回值
;; ----
;; list? 时区名称字符串列表。
;;
;; 示例
;; ----
;; (tz-timezones)

(let ((zones (tz-timezones)))
  (check-true (list? zones))
  (check-true (> (length zones) 0)))

(check-report)
