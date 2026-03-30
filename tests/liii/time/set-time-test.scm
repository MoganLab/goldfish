(import (liii check)
        (liii time)
        (srfi srfi-19)
) ;import

(check-set-mode! 'report-failed)

;; set-time-type! set-time-nanosecond! set-time-second!
;; 设置时间对象的组成部分。
;;
;; 语法
;; ----
;; (set-time-type! time type)
;; (set-time-nanosecond! time nanosecond)
;; (set-time-second! time second)
;;
;; 参数
;; ----
;; time : time? 要修改的时间对象。
;; type : symbol? 新的时间类型。
;; nanosecond : integer? 新的纳秒部分。
;; second : integer? 新的秒部分。
;;
;; 返回值
;; ----
;; any? 返回被设定的新值。
;;
;; 错误处理
;; ----
;; wrong-type-arg 当参数类型不正确时抛出错误。

;; Test set-time-*! procedures
(let ((t (make-time TIME-UTC 0 0)))
  (check (set-time-type! t TIME-MONOTONIC)  => TIME-MONOTONIC)
  (check (set-time-nanosecond! t 555555555) => 555555555)
  (check (set-time-second! t 1234567890)    => 1234567890)

  (check (time-type t) => TIME-MONOTONIC)
  (check (time-nanosecond t) => 555555555)
  (check (time-second t) => 1234567890)
) ;let

;; Test error conditions for set-time-*!
(let ((t (make-time TIME-UTC 0 0)))
  (check-catch 'wrong-type-arg (set-time-type! "not-a-time" TIME-MONOTONIC))
  (check (set-time-type! t 'invalid-type) => 'invalid-type)
  (check-catch 'wrong-type-arg (set-time-nanosecond! "not-a-time" 0))
  (check-catch 'wrong-type-arg (set-time-second! "not-a-time" 0))
) ;let

(check-report)
