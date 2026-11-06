;; offset-rule
;; 若本地时刻落在指定区间内则构造单元素 time-range 向量。
;;
;; 语法
;; ----
;; (offset-rule make-tr time year subindex dst->std std->dst dst-offset std-offset dst-designation std-designation)
;;
;; 参数
;; ----
;; make-tr : procedure? time-range 构造函数。
;; time : integer? 本地时刻。
;; year : integer? 年份。
;; subindex : integer? 区间索引（-1/0/1）。
;; dst->std : procedure? 退出夏令时时刻计算函数。
;; std->dst : procedure? 进入夏令时时刻计算函数。
;; dst-offset : time-delta? 夏令时偏移。
;; std-offset : time-delta? 标准时间偏移。
;; dst-designation : string? 夏令时缩写。
;; std-designation : string? 标准时间缩写。
;;
;; 返回值
;; ----
;; vector? 命中区间时为单元素 time-range 向量，否则为空向量。

(import (liii check)
  (liii date-time date)
  (liii date-time time-delta)
  (liii date-time internal date-rd)
  (liii date-time internal database-types)
  (liii date-time internal tz-types)
  (liii date-time internal dst)
) ;import

(check-set-mode! 'report-failed)

(define std-o (hours-delta 0))

(define dst-o (hours-delta 1))

(define dst-trans
  (tz-transition (tz-julian-without-leap-day 60) (tz-offset #f 2 0 0))
) ;define

(define std-trans
  (tz-transition (tz-julian-without-leap-day 274) (tz-offset #f 2 0 0))
) ;define

(define d2s (lambda (y off) (transition-unix dst-o y std-trans)))

(define s2d (lambda (y off) (transition-unix std-o y dst-trans)))

(define t0
  (+
    (* (- (date->rata-die (make-date 2021 3 1)) unix-epoch-rd) 86400)
    7200
  ) ;+
) ;define

(define v
  (offset-rule time-range (+ t0 100) 2021 0 d2s s2d dst-o std-o "EDT" "EST")
) ;define

(check (vector-length v) => 1)
(check-true (time-range-dst? (vector-ref v 0)))
(check (time-range-designation (vector-ref v 0)) => "EDT")
(check (vector-length (offset-rule time-range (- t0 100) 2021 0 d2s s2d dst-o std-o "EDT" "EST")
       ) ;vector-length
  =>
  0
) ;check

(check-report)
