(import (liii check)
        (liii date-time timezone)
        (liii date-time timestamp))

(check-set-mode! 'report-failed)

;; timestamp-hms
;; 以多值（values）形式返回时间戳对象的时、分、秒。
;;
;; 语法
;; ----
;; (timestamp-hms ts)
;;
;; 参数
;; ----
;; ts : timestamp?
;;
;; 返回值
;; ----
;; values : (values hour minute second)
;;
;; 示例
;; ----
;; (let-values (((h m s) (timestamp-hms (make-timestamp 2020 1 1 10 1 2 (utc-timezone))))) ...)

(let-values (((h m s) (timestamp-hms (make-timestamp 2020 1 1 10 1 2 (utc-timezone)))))
  (check h => 10)
  (check m => 1)
  (check s => 2))

(check-catch 'type-error (timestamp-hms 100102))

(check-report)
