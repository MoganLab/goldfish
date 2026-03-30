(import (liii check)
        (liii time)
        (srfi srfi-19)
) ;import

(check-set-mode! 'report-failed)

;; time-monotonic->time-utc
;; 将 MONOTONIC 时间对象转换为 UTC 时间对象。
;;
;; 语法
;; ----
;; (time-monotonic->time-utc time)
;;
;; 参数
;; ----
;; time : time? TIME-MONOTONIC 类型的时间对象。
;;
;; 返回值
;; ----
;; time? TIME-UTC 类型的时间对象。
;;
;; 错误处理
;; --------
;; wrong-type-arg 当参数不是时间对象或类型不匹配时抛出错误。

;; time-monotonic->time-utc basic
(let* ((t-mon (make-time TIME-MONOTONIC 123456789 42))
       (t-utc (time-monotonic->time-utc t-mon)))
  (check (time-type t-utc) => TIME-UTC)
  (check (time-second t-utc) => 42)
  (check (time-nanosecond t-utc) => 123456789)
) ;let*

;; round-trip
(let* ((t-utc1 (make-time TIME-UTC 123456789 42))
       (t-mon (time-utc->time-monotonic t-utc1))
       (t-utc2 (time-monotonic->time-utc t-mon)))
  (check (time-type t-utc2) => TIME-UTC)
  (check (time-second t-utc2) => 42)
  (check (time-nanosecond t-utc2) => 123456789)
) ;let*

(check-report)
