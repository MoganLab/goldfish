(import (liii check)
        (liii date-time date)
        (liii date-time clock-time)
        (liii date-time timezone)
        (liii date-time timestamp))

(check-set-mode! 'report-failed)

;; date+clock-time->timestamp
;; 通过 date 对象和 clock-time 对象组合创建 timestamp。
;;
;; 语法
;; ----
;; (date+clock-time->timestamp date clock-time timezone [fold])
;;
;; 参数
;; ----
;; date : date?
;; clock-time : clock-time?
;; timezone : timezone?
;; fold : integer? 可选，0 或 1。
;;
;; 返回值
;; ----
;; timestamp?
;;
;; 示例
;; ----
;; (date+clock-time->timestamp (make-date 2020 1 1) (make-clock-time 10 0 0) (utc-timezone))

(check-true (timestamp? (date+clock-time->timestamp (make-date 2020 1 1)
                                                    (make-clock-time 10 0 0)
                                                    (utc-timezone))))
(check-true (timestamp? (date+clock-time->timestamp (make-date 2020 1 1)
                                                    (make-clock-time 10 0 0)
                                                    (utc-timezone)
                                                    0)))

(check-catch 'type-error (date+clock-time->timestamp "2020-01-01" (make-clock-time 10 0 0) (utc-timezone)))
(check-catch 'type-error (date+clock-time->timestamp (make-date 2020 1 1) "10:00:00" (utc-timezone)))

(check-report)
