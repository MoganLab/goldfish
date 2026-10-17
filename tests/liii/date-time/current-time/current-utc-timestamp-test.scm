(import (liii check)
        (liii date-time timestamp)
        (liii date-time current-time))

(check-set-mode! 'report-failed)

;; current-utc-timestamp
;; 获取系统当前 UTC 时区下的时间戳。
;;
;; 语法
;; ----
;; (current-utc-timestamp)
;;
;; 返回值
;; ----
;; timestamp?
;;
;; 示例
;; ----
;; (current-utc-timestamp)

(check-true (timestamp? (current-utc-timestamp)))

(check-report)
