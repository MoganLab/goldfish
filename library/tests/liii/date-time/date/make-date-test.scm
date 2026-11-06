(import (liii check)
        (liii date-time date))

(check-set-mode! 'report-failed)

;; make-date
;; 创建一个公历日期对象。
;;
;; 语法
;; ----
;; (make-date year month day)
;;
;; 参数
;; ----
;; year : integer? 年份（可为负数或 0）。
;; month : integer? 月份（1-12）。
;; day : integer? 日期（必须是该年该月的有效日期）。
;;
;; 返回值
;; ----
;; date? 新创建的日期对象。
;;
;; 示例
;; ----
;; (make-date 2020 1 1)
;; (make-date 2020 2 29)
;;
;; 错误处理
;; --------
;; type-error 当参数不是整数或日期不合法时抛出错误。

(check-true (date? (make-date 2020 1 1)))
(check-true (date? (make-date 2020 2 29)))
(check-true (date? (make-date 2000 2 29)))
(check-true (date? (make-date 0 1 1)))
(check-true (date? (make-date -1 12 31)))

;; 校验非法日期
(check-catch 'type-error (make-date 2020 1 32))
(check-catch 'type-error (make-date 2020 1 0))
(check-catch 'type-error (make-date 2020 1 -1))
(check-catch 'type-error (make-date 2020 0 1))
(check-catch 'type-error (make-date 2020 13 1))
(check-catch 'type-error (make-date 1900 2 29))
(check-catch 'type-error (make-date 2021 2 29))
(check-catch 'type-error (make-date 2020.1 1 1))
(check-catch 'type-error (make-date 2020 1.1 1))
(check-catch 'type-error (make-date 2020 1 1.1))
(check-catch 'type-error (make-date 'foo 1 1))

(check-report)
