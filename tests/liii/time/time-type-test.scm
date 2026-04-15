(import (liii check)
  (liii time)
  (srfi srfi-19)
) ;import


(check-set-mode! 'report-failed)


;; time-type
;; 获取时间对象的类型。
;;
;; 语法
;; ----
;; (time-type time)
;;
;; 参数
;; ----
;; time : time?
;; 时间对象。
;;
;; 返回值
;; ----
;; symbol?
;; 时间类型（如 TIME-UTC, TIME-TAI, TIME-MONOTONIC 等）。
;;
;; 示例
;; ----
;; (time-type (make-time TIME-UTC 0 0)) => time-utc
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
  (check (time-type t1) => TIME-UTC)
  (check (time-type t2) => TIME-MONOTONIC)
  (check (time-type t3) => TIME-TAI)
) ;let


(check-catch 'wrong-type-arg
  (time-type "not-a-time")
) ;check-catch


(check-report)
