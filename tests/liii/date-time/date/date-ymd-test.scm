(import (liii check)
        (liii date-time date))

(check-set-mode! 'report-failed)

;; date-ymd
;; 以多值（values）形式返回日期对象的年、月、日。
;;
;; 语法
;; ----
;; (date-ymd date)
;;
;; 参数
;; ----
;; date : date? 日期对象。
;;
;; 返回值
;; ----
;; values : (values year month day)
;;
;; 示例
;; ----
;; (let-values (((y m d) (date-ymd (make-date 2000 1 2)))) (list y m d)) => '(2000 1 2)
;;
;; 错误处理
;; --------
;; type-error 当参数不是日期对象时抛出错误。

(let-values (((y m d) (date-ymd (make-date 2000 1 2))))
  (check y => 2000)
  (check m => 1)
  (check d => 2))

(let-values (((y m d) (date-ymd (make-date -50 12 31))))
  (check y => -50)
  (check m => 12)
  (check d => 31))

(check-catch 'type-error (date-ymd "2000-01-02"))
(check-catch 'type-error (date-ymd 123))

(check-report)
