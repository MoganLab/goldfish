(import (liii check)
        (liii time)
        (srfi srfi-19)
) ;import

(check-set-mode! 'report-failed)

;; set-time-second!
;; 设置时间对象的秒部分。
;;
;; 语法
;; ----
;; (set-time-second! time second)
;;
;; 参数
;; ----
;; time : time? 要修改的时间对象。
;; second : integer? 新的秒部分。
;;
;; 返回值
;; ----
;; integer? 返回被设定的新秒值。
;;
;; 错误处理
;; --------
;; wrong-type-arg 当参数类型不正确时抛出错误。

(let ((t (make-time TIME-UTC 0 0)))
  (check (set-time-second! t 1234567890) => 1234567890)
  (check (time-second t) => 1234567890)
  (check (set-time-second! t 0) => 0)
  (check (time-second t) => 0)
) ;let

;; Test error conditions
(let ((t (make-time TIME-UTC 0 0)))
  (check-catch 'wrong-type-arg (set-time-second! "not-a-time" 0))
) ;let

(check-report)
