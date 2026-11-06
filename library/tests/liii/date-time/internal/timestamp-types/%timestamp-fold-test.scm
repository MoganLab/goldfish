;; %timestamp-fold
;; 读取 timestamp 记录的 fold 折叠字段。
;;
;; 语法
;; ----
;; (%timestamp-fold ts)
;;
;; 参数
;; ----
;; ts : timestamp? 时间戳对象。
;;
;; 返回值
;; ----
;; integer? 折叠标记：0 表示首次或唯一，1 表示回拨后的第二次。

(import (liii check)
  (liii date-time date)
  (liii date-time clock-time)
  (liii date-time timezone)
  (liii date-time internal timestamp-types)
) ;import

(check-set-mode! 'report-failed)

(check (%timestamp-fold (%make-timestamp (make-date 2021 1 2) (make-clock-time 3 4 5) (utc-timezone) 0)
       ) ;%timestamp-fold
  =>
  0
) ;check
(check (%timestamp-fold (%make-timestamp (make-date 2021 1 2) (make-clock-time 3 4 5) (utc-timezone) 1)
       ) ;%timestamp-fold
  =>
  1
) ;check

(check-report)
