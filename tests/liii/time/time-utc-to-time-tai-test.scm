(import (liii check)
        (liii time)
        (srfi srfi-19)
) ;import

(check-set-mode! 'report-failed)

;; time-utc->time-tai
;; 将 UTC 时间对象转换为 TAI 时间对象。
;;
;; 语法
;; ----
;; (time-utc->time-tai time)
;;
;; 参数
;; ----
;; time : time? TIME-UTC 类型的时间对象。
;;
;; 返回值
;; ----
;; time? TIME-TAI 类型的时间对象。
;;
;; 错误处理
;; --------
;; wrong-type-arg 当参数不是时间对象或类型不匹配时抛出错误。

;; time-utc->time-tai basic
(let* ((t-utc (make-time TIME-UTC 123456789 1483228800))
       (t-tai (time-utc->time-tai t-utc)))
  (check (time-type t-tai) => TIME-TAI)
  (check (time-second t-tai) => 1483228837)
  (check (time-nanosecond t-tai) => 123456789)
) ;let*

;; round-trip
(let* ((t-utc1 (make-time TIME-UTC 123456789 1483228800))
       (t-tai (time-utc->time-tai t-utc1))
       (t-utc2 (time-tai->time-utc t-tai)))
  (check (time-type t-utc2) => TIME-UTC)
  (check (time-second t-utc2) => 1483228800)
  (check (time-nanosecond t-utc2) => 123456789)
) ;let*

(check-report)
