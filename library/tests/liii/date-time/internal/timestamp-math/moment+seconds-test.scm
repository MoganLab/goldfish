;; moment+seconds
;; 将物理时刻向前或向后推移给定秒数。
;;
;; 语法
;; ----
;; (moment+seconds m s+)
;;
;; 参数
;; ----
;; m : moment?
;; 起始物理时刻。
;;
;; s+ : number
;; 推移的秒数，可为负数与分数。
;;
;; 返回值
;; ----
;; moment?
;; 推移后的物理时刻；跨日时自动进退日期。
;;
;; 说明
;; ----
;; 推移以“当日秒数 + s+”计算，超出 [0, 86400) 的部分
;; 折算为整天数加到日期上，分数秒部分原样保留。

(import (liii check)
  (liii date-time date)
  (liii date-time moment)
  (liii date-time internal timestamp-math)
) ;import

(check-set-mode! 'report-failed)

(define m (make-moment (make-date 2026 1 2) 3600))

;; 同日推移
(check (moment=? (moment+seconds m 60) (make-moment (make-date 2026 1 2) 3660))
  =>
  #t
) ;check

;; 向后推移跨入前一天
(check (moment=? (moment+seconds m -7200) (make-moment (make-date 2026 1 1) 82800))
  =>
  #t
) ;check

;; 向前推移跨入后一天
(check (moment=? (moment+seconds m 86300) (make-moment (make-date 2026 1 3) 3500))
  =>
  #t
) ;check

;; 分数秒保留
(let ((result (moment+seconds m 1/2)))
  (check (moment-date result) => (make-date 2026 1 2))
  (check (moment-second-of-day result) => (+ 3600 1/2))
) ;let

;; 推移 0 秒不改变时刻
(check (moment=? (moment+seconds m 0) m) => #t)

(check-report)
