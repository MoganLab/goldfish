(import (liii check)
        (liii time)
        (srfi srfi-19)
) ;import

(check-set-mode! 'report-failed)

;; time-second
;; 获取时间对象的秒部分。
;;
;; 语法
;; ----
;; (time-second time)
;;
;; 参数
;; ----
;; time : time?
;; 时间对象。
;;
;; 返回值
;; ----
;; integer?
;; 秒部分。
;;
;; 示例
;; ----
;; (time-second (make-time TIME-UTC 0 987654321)) => 987654321
;;
;; 错误处理
;; ----
;; wrong-type-arg 当参数不是时间对象时抛出错误。

(let ((t1 (make-time TIME-UTC 123456789 987654321))
      (t2 (make-time TIME-MONOTONIC 999999999 0))
      (t3 (make-time TIME-TAI 0 -1234567890)))
  (check (time-second t1) => 987654321)
  (check (time-second t2) => 0)
  (check (time-second t3) => -1234567890)
) ;let

(check-catch 'wrong-type-arg (time-second 'symbol))

(check-report)
