(import (liii check)
        (liii time)
        (srfi srfi-19)
) ;import

(check-set-mode! 'report-failed)

;; set-time-nanosecond!
;; 设置时间对象的纳秒部分。
;;
;; 语法
;; ----
;; (set-time-nanosecond! time nanosecond)
;;
;; 参数
;; ----
;; time : time? 要修改的时间对象。
;; nanosecond : integer? 新的纳秒部分（0-999999999）。
;;
;; 返回值
;; ----
;; integer? 返回被设定的新纳秒值。
;;
;; 错误处理
;; --------
;; wrong-type-arg 当参数类型不正确时抛出错误。

(let ((t (make-time TIME-UTC 0 0)))
  (check (set-time-nanosecond! t 555555555) => 555555555)
  (check (time-nanosecond t) => 555555555)
  (check (set-time-nanosecond! t 0) => 0)
  (check (time-nanosecond t) => 0)
) ;let

;; Test error conditions
(let ((t (make-time TIME-UTC 0 0)))
  (check-catch 'wrong-type-arg (set-time-nanosecond! "not-a-time" 0))
) ;let

(check-report)
