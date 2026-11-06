(import (liii check)
        (liii date-time clock-time))

(check-set-mode! 'report-failed)

;; clock-time-hms
;; 以多值（values）形式返回时钟时间对象的时、分、秒。
;;
;; 语法
;; ----
;; (clock-time-hms ct)
;;
;; 参数
;; ----
;; ct : clock-time? 时钟时间对象。
;;
;; 返回值
;; ----
;; values : (values hour minute second)
;;
;; 示例
;; ----
;; (let-values (((h m s) (clock-time-hms (make-clock-time 23 1 2)))) (list h m s)) => '(23 1 2)
;;
;; 错误处理
;; --------
;; type-error 当参数不是 clock-time 对象时抛出错误。

(let-values (((h m s) (clock-time-hms (make-clock-time 23 1 2))))
  (check h => 23)
  (check m => 1)
  (check s => 2))

(let-values (((h m s) (clock-time-hms (make-clock-time 0 30 15/2))))
  (check h => 0)
  (check m => 30)
  (check s => 15/2))

(check-catch 'type-error (clock-time-hms "invalid"))
(check-catch 'type-error (clock-time-hms 120000))

(check-report)
