(import (liii check)
        (liii date-time timezone))

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

(check-report)
