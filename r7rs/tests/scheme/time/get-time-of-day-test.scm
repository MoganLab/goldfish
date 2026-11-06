;; get-time-of-day
;; 获取当前的秒数与微秒数。
;;
;; 语法
;; ----
;; (get-time-of-day)
;;
;; 参数
;; ----
;; 无参数。
;;
;; 返回值
;; ----
;; integer integer
;; 两个值：自 Unix 纪元以来的整秒数，以及不足一秒的微秒余数。

(import (liii check) (scheme base) (scheme time))

(check-set-mode! 'report-failed)

(let-values (((sec usec) (get-time-of-day)))
  (check (integer? sec) => #t)
  (check (integer? usec) => #t)
  (check (and (>= usec 0) (< usec 1000000)) => #t)
  ;; 当前时间应晚于 2020-01-01
  (check (> sec 1577836800) => #t)
) ;let-values

;; 连续读取的秒数不应回退
(let-values (((sec1 _) (get-time-of-day)))
  (let-values (((sec2 _) (get-time-of-day)))
    (check (>= sec2 sec1) => #t)
  ) ;let-values
) ;let-values

(check-report)
