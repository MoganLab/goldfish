(import (liii check)
  (liii time)
  (srfi srfi-19)
) ;import


(check-set-mode! 'report-failed)


;; time-tai->time-utc
;; 将 TAI 时间对象转换为 UTC 时间对象。
;;
;; 语法
;; ----
;; (time-tai->time-utc time)
;;
;; 参数
;; ----
;; time : time? TIME-TAI 类型的时间对象。
;;
;; 返回值
;; ----
;; time? TIME-UTC 类型的时间对象。
;;
;; 错误处理
;; --------
;; wrong-type-arg 当参数不是时间对象或类型不匹配时抛出错误。


;; time-tai->time-utc basic
(let* ((t-tai (make-time TIME-TAI
                123456789
                1483228837
              ) ;make-time
       ) ;t-tai
       (t-utc (time-tai->time-utc t-tai))
      ) ;
  (check (time-type t-utc) => TIME-UTC)
  (check (time-second t-utc)
    =>
    1483228800
  ) ;check
  (check (time-nanosecond t-utc)
    =>
    123456789
  ) ;check
) ;let*


;; round-trip
(let* ((t-utc1 (make-time TIME-UTC
                 123456789
                 1483228800
               ) ;make-time
       ) ;t-utc1
       (t-tai (time-utc->time-tai t-utc1))
       (t-utc2 (time-tai->time-utc t-tai))
      ) ;
  (check (time-type t-utc2) => TIME-UTC)
  (check (time-second t-utc2)
    =>
    1483228800
  ) ;check
  (check (time-nanosecond t-utc2)
    =>
    123456789
  ) ;check
) ;let*


(check-report)
