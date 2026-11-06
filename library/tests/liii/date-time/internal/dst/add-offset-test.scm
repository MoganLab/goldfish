;; add-offset
;; 把 time-delta 偏移量加到 UNIX 秒时间戳上。
;;
;; 语法
;; ----
;; (add-offset unix offset)
;;
;; 参数
;; ----
;; unix : integer? UNIX 秒时间戳。
;; offset : time-delta? 偏移量。
;;
;; 返回值
;; ----
;; integer? 加上偏移后的时间戳。

(import (liii check) (liii date-time time-delta) (liii date-time internal dst))

(check-set-mode! 'report-failed)

(check (add-offset 0 (hours-delta 1)) => 3600)
(check (add-offset 0 (time-delta+ (hours-delta 1) (minutes-delta 30))) => 5400)
(check (add-offset 7200 (hours-delta -1)) => 3600)

(check-report)
