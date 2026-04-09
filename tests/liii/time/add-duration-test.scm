(import (liii check)
        (liii time)
        (srfi srfi-19)
) ;import

(check-set-mode! 'report-failed)

;; add-duration
;; 将时间间隔加到时间对象。
;;
;; 语法
;; ----
;; (add-duration time1 time-duration)
;;
;; 参数
;; ----
;; time1 : time? 基准时间对象。
;; time-duration : time? TIME-DURATION 类型的时间对象。
;;
;; 返回值
;; ----
;; time? 返回一个与 time1 同类型的新时间对象。
;;
;; 示例
;; ----
;; (add-duration (make-time TIME-UTC 900000000 3) (time-difference t1 t2))
;;
;; 错误处理
;; --------
;; wrong-type-arg 当参数不是时间对象，或 time-duration 不是 TIME-DURATION 时抛出错误。

;; Test add-duration basic
(let* ((t1 (make-time TIME-UTC 100 5))
       (t2 (make-time TIME-UTC 900000000 3))
       (d  (time-difference t1 t2))
       (t3 (add-duration t2 d)))
  (check (time-type t3) => TIME-UTC)
  (check (time-second t3) => 5)
  (check (time-nanosecond t3) => 100)
) ;let*

;; Test negative duration normalization
(let* ((t1 (make-time TIME-UTC 100 5))
       (t2 (make-time TIME-UTC 900000000 5))
       (d  (time-difference t1 t2))
       (t3 (add-duration t2 d)))
  (check (time-second t3) => 5)
  (check (time-nanosecond t3) => 100)
) ;let*

;; Test error conditions
(let
  ((d
     (time-difference (make-time TIME-UTC 0 1)
                      (make-time TIME-UTC 0 0))
     ) ;time-difference
  ) ;
  (check-catch 'wrong-type-arg (add-duration "not-time" d))
  (check-catch 'wrong-type-arg (add-duration (make-time TIME-UTC 0 0)
                                             (make-time TIME-UTC 0 0))
  ) ;check-catch
) ;let

(check-report)
