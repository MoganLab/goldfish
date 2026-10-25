(import (liii check))

(check-set-mode! 'report-failed)

;; g_windows_tz_to_iana
;; CLDR 标准的 Windows 时区名到 IANA 时区名的映射（内部函数）。
;; 用于 Windows 平台将 GetDynamicTimeZoneInformation 得到的系统时区名
;; （如 "China Standard Time"）映射为 IANA 时区名（如 "Asia/Shanghai"）。
;;
;; 语法
;; ----
;; (g_windows_tz_to_iana windows-tz-name)
;;
;; 参数
;; ----
;; windows-tz-name : string? Windows 时区名
;;
;; 返回值
;; ----
;; string? 对应的 IANA 时区名；未知时区名返回 #f。
;;
;; 示例
;; ----
;; (g_windows_tz_to_iana "China Standard Time") => "Asia/Shanghai"

(check (g_windows_tz_to_iana "China Standard Time") => "Asia/Shanghai")
(check (g_windows_tz_to_iana "Tokyo Standard Time") => "Asia/Tokyo")
(check (g_windows_tz_to_iana "Eastern Standard Time") => "America/New_York")
(check (g_windows_tz_to_iana "Central Europe Standard Time") => "Europe/Budapest")
(check (g_windows_tz_to_iana "GMT Standard Time") => "Europe/London")
(check (g_windows_tz_to_iana "UTC") => "Etc/UTC")
(check (g_windows_tz_to_iana "Dateline Standard Time") => "Etc/GMT+12")

;; 未知的 Windows 时区名返回 #f
(check (g_windows_tz_to_iana "Nonexistent Standard Time") => #f)
(check (g_windows_tz_to_iana "") => #f)

;; 非字符串参数抛出 type-error
(check-catch 'type-error (g_windows_tz_to_iana 42))
(check-catch 'type-error (g_windows_tz_to_iana 'sym))

(check-report)
