;; find-an-offset-rule
;; 在 DST 规则年中定位本地时刻所属的年份与区间索引。
;;
;; 语法
;; ----
;; (find-an-offset-rule local-time dst->std std->dst dst-offset std-offset)
;;
;; 参数
;; ----
;; local-time : integer? 本地时刻。
;; dst->std : procedure? 退出夏令时时刻计算函数。
;; std->dst : procedure? 进入夏令时时刻计算函数。
;; dst-offset : time-delta? 夏令时偏移。
;; std-offset : time-delta? 标准时间偏移。
;;
;; 返回值
;; ----
;; values integer? 年份与 integer? 区间索引（-1/0/1）。

(import (liii check)
  (liii date-time date)
  (liii date-time time-delta)
  (liii date-time internal date-rd)
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

(check
  (call-with-values (lambda () (find-an-offset-rule (+ t0 100) d2s s2d dst-o std-o))
    list
  ) ;call-with-values
  =>
  '(2021 0)
) ;check
(check
  (call-with-values (lambda () (find-an-offset-rule (- t0 100) d2s s2d dst-o std-o))
    list
  ) ;call-with-values
  =>
  '(2021 -1)
) ;check

(check-report)
