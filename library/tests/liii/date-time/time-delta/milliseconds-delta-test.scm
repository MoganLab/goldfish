;; milliseconds-delta
;; 创建 time-delta 对象，毫秒进位到 hours/minutes/seconds 分量。
;; 不足 1 秒的余数以分数秒形式保留在 seconds 分量，不进位到 day 及以上分量。
;;
;; 语法
;; ----
;; (milliseconds-delta n)
;;
;; 参数
;; ----
;; n : integer? 精确整数毫秒数。
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

;; 纯毫秒：不足 1 秒，进入 seconds 分量的分数部分
(let ((dt (milliseconds-delta 500)))
  (check-true (time-delta? dt))
  (check (time-delta-hours dt) => 0)
  (check (time-delta-minutes dt) => 0)
  (check (time-delta-seconds dt) => 1/2)
) ;let

;; 整秒
(let ((dt (milliseconds-delta 5000)))
  (check (time-delta-seconds dt) => 5)
) ;let

;; 亚秒 + 整秒混合：29.4 秒
(let ((dt (milliseconds-delta 29400)))
  (check (time-delta-hours dt) => 0)
  (check (time-delta-minutes dt) => 0)
  (check (time-delta-seconds dt) => 147/5)
) ;let

;; 进位到分钟：90000ms = 1m30s
(let ((dt (milliseconds-delta 90000)))
  (check (time-delta-hours dt) => 0)
  (check (time-delta-minutes dt) => 1)
  (check (time-delta-seconds dt) => 30)
) ;let

;; 整分：720000ms = 12m
(let ((dt (milliseconds-delta 720000)))
  (check (time-delta-minutes dt) => 12)
  (check (time-delta-seconds dt) => 0)
) ;let

;; 进位到小时：3900000ms = 1h5m
(let ((dt (milliseconds-delta 3900000)))
  (check (time-delta-hours dt) => 1)
  (check (time-delta-minutes dt) => 5)
  (check (time-delta-seconds dt) => 0)
) ;let

;; 零
(let ((dt (milliseconds-delta 0)))
  (check (time-delta-hours dt) => 0)
  (check (time-delta-minutes dt) => 0)
  (check (time-delta-seconds dt) => 0)
) ;let

;; 不进位到 day：90000000ms = 25h
(let ((dt (milliseconds-delta 90000000)))
  (check (time-delta-days dt) => 0)
  (check (time-delta-hours dt) => 25)
  (check (time-delta-minutes dt) => 0)
) ;let

(check-catch 'type-error (milliseconds-delta 45.5))
(check-catch 'type-error (milliseconds-delta 'foo))

(check-report)
