(import (liii check)
        (liii date-time time-delta))

(check-set-mode! 'report-failed)

;; years-delta
;; 创建仅包含年份增量的 time-delta 对象。
;;
;; 语法
;; ----
;; (years-delta n)
;;
;; 参数
;; ----
;; n : integer? 精确整数。
;;
;; 返回值
;; ----
;; time-delta?
;;
;; 示例
;; ----
;; (time-delta-years (years-delta 10)) => 10
;;
;; 错误处理
;; --------
;; type-error 当参数不是精确整数时抛出错误。

(let ((dt (years-delta 10)))
  (check-true (time-delta? dt))
  (check (time-delta-years dt) => 10)
  (check (time-delta-months dt) => 0)
  (check (time-delta-weeks dt) => 0)
  (check (time-delta-days dt) => 0)
  (check (time-delta-hours dt) => 0)
  (check (time-delta-minutes dt) => 0)
  (check (time-delta-seconds dt) => 0))

(check-catch 'type-error (years-delta 1.5))
(check-catch 'type-error (years-delta "10"))

(check-report)
