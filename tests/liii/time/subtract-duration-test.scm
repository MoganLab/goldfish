(import (liii check)
  (liii time)
  (srfi srfi-19)
) ;import


(check-set-mode! 'report-failed)


;; subtract-duration
;; 从时间对象减去时间间隔。
;;
;; 语法
;; ----
;; (subtract-duration time1 time-duration)
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
;; 错误处理
;; --------
;; wrong-type-arg 当参数不是时间对象，或 time-duration 不是 TIME-DURATION 时抛出错误。


;; Test subtract-duration basic
(let* ((t1 (make-time TIME-UTC 100 5))
       (t2 (make-time TIME-UTC 900000000 3))
       (d (time-difference t1 t2))
       (t4 (subtract-duration t1 d))
      ) ;
  (check (time-type t4) => TIME-UTC)
  (check (time-second t4) => 3)
  (check (time-nanosecond t4)
    =>
    900000000
  ) ;check
) ;let*


;; Test error conditions
(let ((d (time-difference (make-time TIME-UTC 0 1)
           (make-time TIME-UTC 0 0)
         ) ;time-difference
      ) ;d
     ) ;
  (check-catch 'wrong-type-arg
    (subtract-duration "not-time" d)
  ) ;check-catch
  (check-catch 'wrong-type-arg
    (subtract-duration (make-time TIME-UTC 0 0)
      (make-time TIME-UTC 0 0)
    ) ;subtract-duration
  ) ;check-catch
) ;let


(check-report)
