;; %make-timestamp
;; 不校验时区规则直接构造 timestamp 记录。
;;
;; 语法
;; ----
;; (%make-timestamp date time timezone fold)
;;
;; 参数
;; ----
;; date : date? 日期部分。
;; time : clock-time? 时间部分。
;; timezone : timezone? 时区对象。
;; fold : integer? 回拨折叠标记（0 或 1）。
;;
;; 返回值
;; ----
;; timestamp? 新建的时间戳对象。

(import (liii check)
  (liii date-time date)
  (liii date-time clock-time)
  (liii date-time timezone)
  (liii date-time internal timestamp-types)
) ;import

(check-set-mode! 'report-failed)

(define ts
  (%make-timestamp (make-date 2021 1 2) (make-clock-time 3 4 5) (utc-timezone) 0)
) ;define

(check-true (timestamp? ts))
(check (timestamp-year ts) => 2021)
(check (timestamp-hour ts) => 3)
(check (timestamp-fold ts) => 0)

(check-report)
