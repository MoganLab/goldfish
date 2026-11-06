(import (liii check)
        (liii date-time time-delta))

(check-set-mode! 'report-failed)

;; make-time-delta
;; 创建一个时间增量（time-delta）对象。
;;
;; 语法
;; ----
;; (make-time-delta years months weeks days hours minutes seconds)
;;
;; 参数
;; ----
;; years, months, weeks, days, hours, minutes : integer? 精确整数。
;; seconds : rational? 精确有理数或整数。
;;
;; 返回值
;; ----
;; time-delta? 时间增量对象。
;;
;; 示例
;; ----
;; (make-time-delta 1 2 3 4 5 6 7)
;;
;; 错误处理
;; --------
;; type-error 当任一分量不是精确数字或类型不正确时抛出错误。

(check-true (time-delta? (make-time-delta 1 2 3 4 5 6 7)))
(check-true (time-delta? (make-time-delta 0 0 0 0 0 0 1/2)))
(check-true (time-delta? (make-time-delta -1 -2 -3 -4 -5 -6 -7)))

(check-catch 'type-error (make-time-delta 1.5 2 3 4 5 6 7))
(check-catch 'type-error (make-time-delta 1 2 3 4 5 6 7.5))
(check-catch 'type-error (make-time-delta "1" 2 3 4 5 6 7))

(check-report)
