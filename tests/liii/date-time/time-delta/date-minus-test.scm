(import (liii check)
        (liii date-time date)
        (liii date-time time-delta))

(check-set-mode! 'report-failed)

;; date-
;; 计算日期减去时间增量。
;;
;; 语法
;; ----
;; (date- date time-delta)
;;
;; 参数
;; ----
;; date : date? 日期对象。
;; time-delta : time-delta? 时间增量（时分秒分量必须为 0）。
;;
;; 返回值
;; ----
;; date?
;;
;; 示例
;; ----
;; (date- (make-date 2021 1 11) (days-delta 10)) => (make-date 2021 1 1)
;;
;; 错误处理
;; --------
;; type-error 当参数类型不符或增量包含非零的时分秒时抛出错误。

(check-true (date=? (make-date 2021 1 1)
                    (date- (make-date 2021 1 11) (days-delta 10))))

(check-true (date=? (make-date 2020 12 31)
                    (date- (make-date 2021 1 1) (days-delta 1))))

(check-catch 'type-error (date- (make-date 2021 1 1) (hours-delta 1)))

(check-report)
