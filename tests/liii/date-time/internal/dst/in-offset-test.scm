;; in-offset
;; 判断本地时刻相对某年 DST 转移区间的位置。
;;
;; 语法
;; ----
;; (in-offset time year subindex dst->std std->dst dst-offset std-offset)
;;
;; 参数
;; ----
;; time : integer? 待判断的时刻。
;; year : integer? 参考年份。
;; subindex : integer? 区间索引（-1/0/1）。
;; dst->std : procedure? 退出夏令时时刻计算函数。
;; std->dst : procedure? 进入夏令时时刻计算函数。
;; dst-offset : time-delta? 夏令时偏移。
;; std-offset : time-delta? 标准时间偏移。
;;
;; 返回值
;; ----
;; integer? 或 #f：-1 区间之前、0 区间之内、1 区间之后、#f 表示处于缺口。

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

(check (in-offset (- t0 100) 2021 0 d2s s2d dst-o std-o) => -1)
(check (in-offset (+ t0 100) 2021 0 d2s s2d dst-o std-o) => 0)
(check (in-offset (+ t0 100) 2021 -1 d2s s2d dst-o std-o) => 1)
(check (in-offset (+ t0 100) 2021 1 d2s s2d dst-o std-o) => -1)
(check-catch 'type-error (in-offset t0 2021 2 d2s s2d dst-o std-o))

(check-report)
