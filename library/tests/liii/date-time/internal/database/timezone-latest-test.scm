;; timezone-latest
;; 获取该时区支持的最晚时刻（moment）。
;;
;; 语法
;; ----
;; (timezone-latest tz)
;;
;; 参数
;; ----
;; tz : timezone?
;;
;; 返回值
;; ----
;; moment?

(import (liii check) (liii date-time moment) (liii date-time timezone))

(check-set-mode! 'report-failed)


(check-true (moment? (timezone-latest (tz-timezone "Europe/Vilnius"))))

(check-report)
