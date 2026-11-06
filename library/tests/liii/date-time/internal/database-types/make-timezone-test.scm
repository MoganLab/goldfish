;; make-timezone
;; 构造时区数据记录。
;;
;; 语法
;; ----
;; (make-timezone first utc-ranges local-ranges rule)
;;
;; 参数
;; ----
;; first : time-range? 或 #f 首个区间（LMT）。
;; utc-ranges : vector? 按 UTC 时刻索引的区间向量。
;; local-ranges : vector? 按本地时刻索引的区间向量。
;; rule : tzrule? 或 #f POSIX 回退规则。
;;
;; 返回值
;; ----
;; timezone? 新建的时区记录。

(import (liii check)
  (liii date-time time-delta)
  (liii date-time internal database-types)
) ;import

(check-set-mode! 'report-failed)

(define r (time-range 100 200 (hours-delta 8) #t "CST"))

(define utc-v (vector r))

(define loc-v (vector r))

(define tz (make-timezone "Test" utc-v loc-v #f))

(check-true (timezone? tz))

(check-report)
