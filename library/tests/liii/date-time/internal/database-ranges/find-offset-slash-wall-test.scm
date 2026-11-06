;; find-offset/wall
;; 按本地墙钟时刻查找时区中匹配的时间区间集合。
;;
;; 语法
;; ----
;; (find-offset/wall tz local-time)
;;
;; 参数
;; ----
;; tz : timezone? 时区记录。
;; local-time : integer? 本地秒时刻。
;;
;; 返回值
;; ----
;; vector? 匹配的 time-range 向量；回拨时刻可能含多个区间。

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

(check (vector-length (find-offset/wall tz 500)) => 2)
(check (vector-length (find-offset/wall tz 999999)) => 1)
(check (time-range-designation (vector-ref (find-offset/wall tz 999999) 0))
  =>
  "EDT"
) ;check

(check-report)
