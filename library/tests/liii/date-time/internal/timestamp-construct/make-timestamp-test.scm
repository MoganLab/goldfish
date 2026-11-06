;; make-timestamp
;; 创建带时区的时间戳对象。
;;
;; 语法
;; ----
;; (make-timestamp year month day hour minute second timezone [fold])
;;
;; 参数
;; ----
;; year, month, day : integer?
;; hour, minute : integer?
;; second : rational?
;; timezone : timezone?
;; fold : integer? 可选，0 或 1，用于区分夏令时结束时重叠的时钟时间。
;;
;; 返回值
;; ----
;; timestamp?
;;
;; 错误处理
;; --------
;; type-error 当参数无效或该本地时间由于夏令时跳过而不存在时抛出错误。

(import (liii check)
  (liii date-time date)
  (liii date-time timezone)
  (liii date-time timestamp)
) ;import

(check-set-mode! 'report-failed)


(check-true (timestamp? (make-timestamp 2020 1 1 10 0 0 (tz-timezone "Europe/Vilnius")))
) ;check-true
(check-true (timestamp? (make-timestamp 2020 1 1 10 0 0 (tz-timezone "Europe/Vilnius") 0))
) ;check-true

;; 夏令时回退产生重合时段，fold 0 与 1 均合法
(check-true (timestamp? (make-timestamp 2025 10 26 3 30 0 (tz-timezone "Europe/Vilnius") 0))
) ;check-true
(check-true (timestamp? (make-timestamp 2025 10 26 3 30 0 (tz-timezone "Europe/Vilnius") 1))
) ;check-true

;; 时钟前拨跳过 03:00-04:00，该时间在当地不存在，抛出错误
(check-catch 'type-error
  (make-timestamp 2025 3 30 3 30 0 (tz-timezone "Europe/Vilnius"))
) ;check-catch

;; 非重叠时段 fold 必须为 0，不能为 1
(check-catch 'type-error
  (make-timestamp 2020 1 1 10 0 0 (tz-timezone "Europe/Vilnius") 1)
) ;check-catch

;; 合法闰秒
(check-true (timestamp? (make-timestamp 2015 7 1 2 59 60 (tz-timezone "Europe/Vilnius")))
) ;check-true
(check-true (timestamp? (make-timestamp 2015 7 1 2 59 (+ 60 1/2) (tz-timezone "Europe/Vilnius"))
            ) ;timestamp?
) ;check-true

;; 非法闰秒
(check-catch 'type-error
  (make-timestamp 2015 7 1 2 58 60 (tz-timezone "Europe/Vilnius"))
) ;check-catch

(check-report)
