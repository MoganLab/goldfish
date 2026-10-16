(import (liii check)
        (liii date-time date))

(check-set-mode! 'report-failed)

;; rata-die->date
;; 将 Rata Die 日序数转换为公历日期对象。
;;
;; 语法
;; ----
;; (rata-die->date rd)
;;
;; 参数
;; ----
;; rd : integer? Rata Die 日序数。
;;
;; 返回值
;; ----
;; date? 对应的日期对象。
;;
;; 示例
;; ----
;; (rata-die->date 1) => (make-date 1 1 1)
;; (rata-die->date 739539) => (make-date 2025 10 15)
;;
;; 错误处理
;; --------
;; type-error 当参数不是整数时抛出错误。

(check-true (date=? (rata-die->date 1) (make-date 1 1 1)))
(check-true (date=? (rata-die->date 739539) (make-date 2025 10 15)))
(check-true (date=? (rata-die->date -1) (make-date 0 12 30)))
(check-true (date=? (rata-die->date -365) (make-date 0 1 1)))
(check-true (date=? (rata-die->date -366) (make-date -1 12 31)))

(check-catch 'type-error (rata-die->date 1.5))
(check-catch 'type-error (rata-die->date "739539"))
(check-catch 'type-error (rata-die->date 'foo))

(check-report)
