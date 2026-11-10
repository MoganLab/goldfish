;; find-offset/utc
;; 按 UNIX 时刻查找时区中对应的时间区间。
;;
;; 语法
;; ----
;; (find-offset/utc tz unix-time)
;;
;; 参数
;; ----
;; tz : timezone? 时区记录。
;; unix-time : integer? UNIX 秒时刻。
;;
;; 返回值
;; ----
;; time-range? 覆盖该时刻的区间；越界时返回首个区间。

(import (liii check)
  (liii date-time time-delta)
  (liii date-time internal tzif)
  (liii date-time internal database-types)
  (liii date-time internal database-ranges)
) ;import

(check-set-mode! 'report-failed)

(define first-trans (tzif-transition 1000 3600 #t "EDT" #f #f))

(define trans
  (vector (tzif-transition 0 0 #f "UTC" #f #f)
    (tzif-transition 1000 3600 #t "EDT" #f #f)
  ) ;vector
) ;define

(define tz
  (call-with-values (lambda () (build-ranges first-trans trans))
    (lambda (u l) (make-timezone "Test" u l #f))
  ) ;call-with-values
) ;define

(check (time-range-start (find-offset/utc tz 500)) => 0)
(check (time-range-designation (find-offset/utc tz 1500)) => "EDT")
(check (time-range-start (find-offset/utc tz -5)) => #f)

(check-report)
