(import (liii check)
  (liii time)
  (srfi srfi-19)
) ;import


(check-set-mode! 'report-failed)


;; time-nanosecond
;; 获取时间对象的纳秒部分。
;;
;; 语法
;; ----
;; (time-nanosecond time)
;;
;; 参数
;; ----
;; time : time?
;; 时间对象。
;;
;; 返回值
;; ----
;; integer?
;; 纳秒部分（0-999999999）。
;;
;; 示例
;; ----
;; (time-nanosecond (make-time TIME-UTC 123456789 0)) => 123456789
;;
;; 错误处理
;; ----
;; wrong-type-arg 当参数不是时间对象时抛出错误。


(let ((t1 (make-time TIME-UTC 123456789 987654321)
      ) ;t1
      (t2 (make-time TIME-MONOTONIC 999999999 0)
      ) ;t2
      (t3 (make-time TIME-TAI 0 -1234567890))
     ) ;
  (check (time-nanosecond t1)
    =>
    123456789
  ) ;check
  (check (time-nanosecond t2)
    =>
    999999999
  ) ;check
  (check (time-nanosecond t3) => 0)
) ;let


(check-catch 'wrong-type-arg
  (time-nanosecond 123)
) ;check-catch


(check-report)
