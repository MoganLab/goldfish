(import (liii check)
        (liii date-time time-delta))

(check-set-mode! 'report-failed)

;; time-delta-negate
;; 将 time-delta 对象的所有分量取反。
;;
;; 语法
;; ----
;; (time-delta-negate dt)
;;
;; 参数
;; ----
;; dt : time-delta?
;;
;; 返回值
;; ----
;; time-delta? 取反后的 time-delta 对象。
;;
;; 示例
;; ----
;; (time-delta-negate (days-delta 5)) => (days-delta -5)
;;
;; 错误处理
;; --------
;; type-error 当参数不是 time-delta 对象时抛出错误。

(let* ((dt (time-delta+ (years-delta 1) (months-delta 2) (days-delta 3)))
       (neg (time-delta-negate dt)))
  (check (time-delta-years neg) => -1)
  (check (time-delta-months neg) => -2)
  (check (time-delta-days neg) => -3))

(check-catch 'type-error (time-delta-negate 'foo))

(check-report)
