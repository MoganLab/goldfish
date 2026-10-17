(import (liii check)
        (liii date-time date))

(check-set-mode! 'report-failed)

;; date-year
;; 获取日期对象的年份。
;;
;; 语法
;; ----
;; (date-year date)
;;
;; 参数
;; ----
;; date : date? 日期对象。
;;
;; 返回值
;; ----
;; integer? 年份。
;;
;; 示例
;; ----
;; (date-year (make-date 2000 1 2)) => 2000
;;
;; 错误处理
;; --------
;; type-error 当参数不是日期对象时抛出错误。

(check (date-year (make-date 2000 1 2)) => 2000)
(check (date-year (make-date -500 6 15)) => -500)
(check (date-year (make-date 0 1 1)) => 0)

(check-catch 'type-error (date-year "2000-01-02"))
(check-catch 'type-error (date-year 2000))
(check-catch 'type-error (date-year #f))

(check-report)
