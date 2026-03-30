(import (liii check)
        (liii time)
        (srfi srfi-19)
) ;import

(check-set-mode! 'report-failed)

;; time-type time-nanosecond time-second
;; 获取时间对象的组成部分。
;;
;; 语法
;; ----
;; (time-type time)
;; (time-nanosecond time)
;; (time-second time)
;;
;; 参数
;; ----
;; time : time?
;; 时间对象。
;;
;; 返回值
;; ----
;; time-type : symbol? 时间类型。
;; time-nanosecond : integer? 纳秒部分（0-999999999）。
;; time-second : integer? 秒部分。
;;
;; 错误处理
;; ----
;; wrong-type-arg 当参数不是时间对象时抛出错误。

;; Test time accessors
(let ((t1 (make-time TIME-UTC 123456789 987654321))
      (t2 (make-time TIME-MONOTONIC 999999999 0))
      (t3 (make-time TIME-TAI 0 -1234567890)))
  (check (time-type t1) => TIME-UTC)
  (check (time-nanosecond t1) => 123456789)
  (check (time-second t1) => 987654321)

  (check (time-type t2) => TIME-MONOTONIC)
  (check (time-nanosecond t2) => 999999999)
  (check (time-second t2) => 0)

  (check (time-type t3) => TIME-TAI)
  (check (time-nanosecond t3) => 0)
  (check (time-second t3) => -1234567890)
) ;let

;; Test error conditions
(check-catch 'wrong-type-arg (time-type "not-a-time"))
(check-catch 'wrong-type-arg (time-nanosecond 123))
(check-catch 'wrong-type-arg (time-second 'symbol))

(check-report)
