;; make-timestamp-comparator
;; 基于物理时刻构造时间戳链式比较器。
;;
;; 语法
;; ----
;; (make-timestamp-comparator pred)
;;
;; 参数
;; ----
;; pred : procedure? 对 -1/0/1 比较结果判定的谓词。
;;
;; 返回值
;; ----
;; procedure? 接受两个及以上 timestamp 参数、返回 boolean? 的比较器。

(import (liii check)
  (liii date-time time-delta)
  (liii date-time timezone)
  (liii date-time timestamp)
  (liii date-time internal timestamp-cmp)
) ;import

(check-set-mode! 'report-failed)

(define <? (make-timestamp-comparator (lambda (r) (< r 0))))

(define =? (make-timestamp-comparator (lambda (r) (= r 0))))

(define ts1 (make-timestamp 2021 1 1 0 0 0 (utc-timezone)))

(define ts2 (make-timestamp 2021 1 2 0 0 0 (utc-timezone)))

(check-true (<? ts1 ts2))
(check-false (<? ts2 ts1))
(check-true (=? ts1 (make-timestamp 2021 1 1 0 0 0 (utc-timezone))))
(check-true (<? ts1 ts2 (make-timestamp 2021 1 3 0 0 0 (utc-timezone))))
(check-catch 'type-error (<? ts1 "ts"))

(check-report)
