;; months-delta
;; 创建仅包含月份增量的 time-delta 对象。
;;
;; 语法
;; ----
;; (months-delta n)
;;
;; 参数
;; ----
;; n : integer? 精确整数。
;;
;; 返回值
;; ----
;; time-delta?
;;
;; 错误处理
;; --------
;; type-error 当参数不是精确整数时抛出错误。

(import (liii check) (liii date-time time-delta))

(check-set-mode! 'report-failed)


(let ((dt (months-delta 5)))
  (check-true (time-delta? dt))
  (check (time-delta-months dt) => 5)
  (check (time-delta-years dt) => 0)
) ;let

(check-catch 'type-error (months-delta 2.5))

(check-report)
