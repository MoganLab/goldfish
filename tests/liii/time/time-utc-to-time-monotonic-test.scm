(import (liii check)
  (liii time)
  (srfi srfi-19)
) ;import


(check-set-mode! 'report-failed)


;; time-utc->time-monotonic
;; 将 UTC 时间对象转换为 MONOTONIC 时间对象。
;;
;; 语法
;; ----
;; (time-utc->time-monotonic time)
;;
;; 参数
;; ----
;; time : time? TIME-UTC 类型的时间对象。
;;
;; 返回值
;; ----
;; time? TIME-MONOTONIC 类型的时间对象。
;;
;; 错误处理
;; --------
;; wrong-type-arg 当参数不是时间对象或类型不匹配时抛出错误。


;; time-utc->time-monotonic basic
(let* ((t-utc (make-time TIME-UTC 123456789 42)
       ) ;t-utc
       (t-mon (time-utc->time-monotonic t-utc))
      ) ;
  (check (time-type t-mon)
    =>
    TIME-MONOTONIC
  ) ;check
  (check (time-second t-mon) => 42)
  (check (time-nanosecond t-mon)
    =>
    123456789
  ) ;check
) ;let*


;; round-trip
(let* ((t-utc1 (make-time TIME-UTC 123456789 42)
       ) ;t-utc1
       (t-mon (time-utc->time-monotonic t-utc1)
       ) ;t-mon
       (t-utc2 (time-monotonic->time-utc t-mon)
       ) ;t-utc2
      ) ;
  (check (time-type t-utc2) => TIME-UTC)
  (check (time-second t-utc2) => 42)
  (check (time-nanosecond t-utc2)
    =>
    123456789
  ) ;check
) ;let*


(check-report)
