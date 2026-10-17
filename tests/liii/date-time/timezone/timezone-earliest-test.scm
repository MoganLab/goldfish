(import (liii check)
        (liii date-time moment)
        (liii date-time timezone))

(check-set-mode! 'report-failed)

;; timezone-earliest
;; 获取该时区支持的最早时刻（moment）。
;;
;; 语法
;; ----
;; (timezone-earliest tz)
;;
;; 参数
;; ----
;; tz : timezone?
;;
;; 返回值
;; ----
;; moment?
;;
;; 示例
;; ----
;; (timezone-earliest (tz-timezone "Europe/Vilnius"))

(check-true (moment? (timezone-earliest (tz-timezone "Europe/Vilnius"))))

(check-report)
