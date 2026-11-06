(import (liii check)
        (liii date-time timezone)
        (liii date-time timestamp))

(check-set-mode! 'report-failed)

;; timestamp-ymd
;; 以多值（values）形式返回时间戳对象的年、月、日。
;;
;; 语法
;; ----
;; (timestamp-ymd ts)
;;
;; 参数
;; ----
;; ts : timestamp?
;;
;; 返回值
;; ----
;; values : (values year month day)
;;
;; 示例
;; ----
;; (let-values (((y m d) (timestamp-ymd (make-timestamp 2020 1 2 10 0 0 (utc-timezone))))) ...)

(let-values (((y m d) (timestamp-ymd (make-timestamp 2020 1 2 10 0 0 (utc-timezone)))))
  (check y => 2020)
  (check m => 1)
  (check d => 2))

(check-catch 'type-error (timestamp-ymd 'foo))

(check-report)
