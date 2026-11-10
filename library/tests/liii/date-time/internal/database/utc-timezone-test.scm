;; utc-timezone
;; 获取 UTC 协调世界时时区对象。
;;
;; 语法
;; ----
;; (utc-timezone)
;;
;;
;; 参数
;; ----
;; 无。
;; 返回值
;; ----
;; timezone? UTC 时区对象。

(import (liii check) (liii date-time timezone))

(check-set-mode! 'report-failed)


(check-true (timezone? (utc-timezone)))

(check-report)
