(import (liii check)
        (liii date-time timezone)
        (liii date-time timestamp))

(check-set-mode! 'report-failed)

;; timestamp-fold
;; 获取时间戳对象的 fold 标志（0 或 1）。
;;
;; 语法
;; ----
;; (timestamp-fold ts)
;;
;; 参数
;; ----
;; ts : timestamp?
;;
;; 返回值
;; ----
;; integer? 0 或 1。
;;
;; 示例
;; ----
;; (timestamp-fold (make-timestamp 2025 10 26 3 30 0 (tz-timezone "Europe/Vilnius") 1)) => 1

(check (timestamp-fold (make-timestamp 2020 1 1 10 0 0 (utc-timezone))) => 0)
(check (timestamp-fold (make-timestamp 2025 10 26 3 30 0 (tz-timezone "Europe/Vilnius") 0)) => 0)
(check (timestamp-fold (make-timestamp 2025 10 26 3 30 0 (tz-timezone "Europe/Vilnius") 1)) => 1)

(check-catch 'type-error (timestamp-fold 'foo))

(check-report)
