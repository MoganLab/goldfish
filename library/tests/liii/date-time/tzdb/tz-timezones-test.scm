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

;; 内嵌时区注册表：C++ 与 Scheme 回退名单的单一来源
(let ((embedded (g_embedded_zoneinfo_names)))
  (check-true (list? embedded))
  (check-true (and (member "UTC" embedded) #t))
  (check-true (and (member "Etc/UTC" embedded) #t))
  (check-true (and (member "Europe/Vilnius" embedded) #t))
  (check-true (and (member "America/New_York" embedded) #t)))

(check-report)
