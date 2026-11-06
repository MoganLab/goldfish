;; time-delta->seconds
;; 将时间间隔换算为总秒数。
;;
;; 语法
;; ----
;; (time-delta->seconds dt)
;;
;; 参数
;; ----
;; dt : time-delta?
;; 待换算的时间间隔。
;;
;; 返回值
;; ----
;; number
;; 天、时、分、秒字段换算后的总秒数；年与月字段不参与换算。
;;
;; 说明
;; ----
;; 换算规则为 1 天 = 86400 秒、1 小时 = 3600 秒、1 分钟 = 60 秒，
;; 各字段相加，负数字段会产生负秒数。

(import (liii check)
  (liii date-time time-delta)
  (liii date-time internal timestamp-math)
) ;import

(check-set-mode! 'report-failed)

(check (time-delta->seconds (make-time-delta 0 0 0 0 0 0 0)) => 0)

(define dt
  (time-delta+ (days-delta 2) (hours-delta 3) (minutes-delta 4) (seconds-delta 5))
) ;define

(check (time-delta->seconds dt) => (+ (* 2 86400) (* 3 3600) (* 4 60) 5))

(check (time-delta->seconds (seconds-delta -90)) => -90)

(check (time-delta->seconds (time-delta-negate dt))
  =>
  (- (+ (* 2 86400) (* 3 3600) (* 4 60) 5))
) ;check

;; 年与月字段不参与秒数换算
(check (time-delta->seconds (time-delta+ (years-delta 1) (months-delta 2)))
  =>
  0
) ;check

(check-report)
