(import (liii check)
  (liii time)
  (srfi srfi-19)
) ;import


(check-set-mode! 'report-failed)


;; time-difference
;; 计算两个时间对象的差值。
;;
;; 语法
;; ----
;; (time-difference time1 time2)
;;
;; 参数
;; ----
;; time1 : time?
;; time2 : time?
;; 两个时间对象，时间类型必须相同。
;;
;; 返回值
;; ----
;; time?
;; 返回一个 TIME-DURATION 时间类型的时间对象。
;;
;; 示例
;; ----
;; (time-difference (make-time TIME-UTC 100 5) (make-time TIME-UTC 900000000 3))
;;   => TIME-DURATION 类型时间对象
;;
;; 错误处理
;; ----
;; wrong-type-arg 当参数不是时间对象或时间类型不匹配时抛出错误。


;; Test time-difference
(let* ((t1 (make-time TIME-UTC 100 5))
       (t2 (make-time TIME-UTC 900000000 3))
       (d (time-difference t1 t2))
      ) ;
  (check (time-type d) => TIME-DURATION)
  (check (time-second d) => 1)
  (check (time-nanosecond d) => 100000100)
) ;let*


;; Test negative duration normalization
(let* ((t1 (make-time TIME-UTC 100 5))
       (t2 (make-time TIME-UTC 900000000 5))
       (d (time-difference t1 t2))
      ) ;
  (check (time-second d) => -1)
  (check (time-nanosecond d) => 100000100)
) ;let*


;; Test zero difference
(let* ((t1 (make-time TIME-UTC 123456789 42))
       (d (time-difference t1 t1))
      ) ;
  (check (time-second d) => 0)
  (check (time-nanosecond d) => 0)
) ;let*


;; Test error conditions
(check-catch 'wrong-type-arg
  (time-difference (make-time TIME-UTC 0 0)
    (make-time TIME-TAI 0 0)
  ) ;time-difference
) ;check-catch
(check-catch 'wrong-type-arg
  (time-difference "not-time"
    (make-time TIME-UTC 0 0)
  ) ;time-difference
) ;check-catch


(check-report)
