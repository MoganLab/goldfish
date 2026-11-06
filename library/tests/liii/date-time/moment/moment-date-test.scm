(import (liii check)
        (liii date-time date)
        (liii date-time moment))

(check-set-mode! 'report-failed)

;; moment-date
;; 获取时刻对象所关联的公历日期。
;;
;; 语法
;; ----
;; (moment-date m)
;;
;; 参数
;; ----
;; m : moment? 时刻对象。
;;
;; 返回值
;; ----
;; date? 公历日期对象。
;;
;; 示例
;; ----
;; (moment-date (make-moment (make-date 2025 1 1) 1)) => (make-date 2025 1 1)
;;
;; 错误处理
;; --------
;; type-error 当参数不是 moment 对象时抛出错误。

(check-true (date=? (moment-date (make-moment (make-date 2025 1 1) 1))
                    (make-date 2025 1 1)))
(check-true (date=? (moment-date (make-moment (make-date 2000 2 29) 500))
                    (make-date 2000 2 29)))

(check-catch 'type-error (moment-date (make-date 2025 1 1)))
(check-catch 'type-error (moment-date 100))

(check-report)
