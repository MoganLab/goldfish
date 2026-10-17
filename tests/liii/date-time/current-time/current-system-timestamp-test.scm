(import (liii check)
        (liii date-time timestamp)
        (liii date-time current-time))

(check-set-mode! 'report-failed)

;; current-system-timestamp
;; 获取系统当前本地时区下的时间戳。
;;
;; 语法
;; ----
;; (current-system-timestamp)
;;
;; 返回值
;; ----
;; timestamp?
;;
;; 示例
;; ----
;; (current-system-timestamp)

(check-true (timestamp? (current-system-timestamp)))

(check-report)
