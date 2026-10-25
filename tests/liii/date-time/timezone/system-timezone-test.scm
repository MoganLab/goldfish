(import (liii check)
        (liii os)
        (liii date-time timezone)
        (liii date-time tzdb))

(check-set-mode! 'report-failed)

;; system-timezone
;; 获取操作系统当前本地时区对象。
;;
;; 语法
;; ----
;; (system-timezone)
;;
;; 返回值
;; ----
;; timezone? 系统本地时区对象。
;;
;; 示例
;; ----
;; (system-timezone)

(check-true (timezone? (system-timezone)))

;; TZ 指向不存在的时区时应降级而非崩溃（issue #1164 现象 3）
(putenv "TZ" "Goldfish/Nonexistent")
(reload-timezone-data!)
(check-true (timezone? (system-timezone)))

;; TZ 指向已知时区时，system-timezone 应使用该时区
(putenv "TZ" "America/New_York")
(reload-timezone-data!)
(check-true (timezone? (system-timezone)))

;; Windows 系统时区名：返回 IANA 时区名字符串或 #f（非 Windows 恒为 #f）
(check-true
  (let ((name (g_system_timezone_name)))
    (or (not name) (string? name))))

;; 恢复环境，不影响后续测试
(unsetenv "TZ")
(reload-timezone-data!)
(check-true (timezone? (system-timezone)))

(check-report)
