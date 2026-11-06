(import (liii check)
        (liii date-time date)
        (liii date-time time-delta))

(check-set-mode! 'report-failed)

;; date+
;; 将日期与时间增量相加。
;; 注意：time-delta 的 hours, minutes, seconds 分量必须为 0，否则报错。
;;
;; 语法
;; ----
;; (date+ date time-delta)
;;
;; 参数
;; ----
;; date : date? 日期对象。
;; time-delta : time-delta? 时间增量（时分秒分量必须为 0）。
;;
;; 返回值
;; ----
;; date? 计算后的新日期对象。
;;
;; 示例
;; ----
;; (date+ (make-date 2021 1 1) (days-delta 10)) => (make-date 2021 1 11)
;;
;; 错误处理
;; --------
;; type-error 当参数类型不符或增量包含非零的时分秒时抛出错误。

(check-true (date=? (make-date 2021 12 31)
                    (date+ (make-date 2021 1 1)
                           (time-delta+ (days-delta -1) (years-delta 1)))))

(check-true (date=? (make-date -2020 1 1)
                    (date+ (make-date -2021 1 1) (years-delta 1))))

(check-true (date=? (make-date 2021 2 28)
                    (date+ (make-date 2021 1 31) (months-delta 1))))

(check-true (date=? (make-date 2021 1 8)
                    (date+ (make-date 2021 1 1) (weeks-delta 1))))

;; 月份与天数溢出进位
(check-true (date=? (make-date 2022 2 1)
                    (date+ (make-date 2021 1 1) (months-delta 13))))
(check-true (date=? (make-date 2022 2 1)
                    (date+ (make-date 2021 1 1) (days-delta 396))))

(check-catch 'type-error (date+ (make-date 2021 1 1) (hours-delta 1)))
(check-catch 'type-error (date+ (make-date 2021 1 1) (minutes-delta 1)))
(check-catch 'type-error (date+ (make-date 2021 1 1) (seconds-delta 1)))
(check-catch 'type-error (date+ "2021-01-01" (days-delta 1)))

(check-report)
