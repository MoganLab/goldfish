;; build-ranges
;; 由 TZif 转移向量构造 UTC 与本地两套时间区间向量。
;;
;; 语法
;; ----
;; (build-ranges first-trans transitions)
;;
;; 参数
;; ----
;; first-trans : tzif-transition? 或 #f 首个转移之前的信息。
;; transitions : vector? TZif 转移向量。
;;
;; 返回值
;; ----
;; values vector? 两个：按 UTC 与按本地时刻索引的 time-range 向量。

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

(check-true
  (vector? (call-with-values (lambda () (build-ranges first-trans trans)) (lambda (u l) u))
  ) ;vector?
) ;check-true
(call-with-values (lambda () (build-ranges first-trans trans))
  (lambda (u l)
    (check (vector-length u) => 3)
    (check (vector-length l) => 3)
    (check (time-range-start (vector-ref u 1)) => 0)
    (check (time-range-start (vector-ref l 2)) => 4600)
  ) ;lambda
) ;call-with-values
(call-with-values (lambda () (build-ranges #f (vector)))
  (lambda (u l)
    (check (vector-length u) => 1)
    (check (time-range-designation (vector-ref u 0)) => "UTC")
  ) ;lambda
) ;call-with-values

(check-report)
