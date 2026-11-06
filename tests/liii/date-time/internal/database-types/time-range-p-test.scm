;; time-range?
;; 判断对象是否为时间区间记录。
;;
;; 语法
;; ----
;; (time-range? obj)
;;
;; 参数
;; ----
;; obj : any? 任意对象。
;;
;; 返回值
;; ----
;; boolean? 是该类记录返回 #t。

(import (liii check)
  (liii date-time time-delta)
  (liii date-time internal database-types)
) ;import

(check-set-mode! 'report-failed)

(define r (time-range 100 200 (hours-delta 8) #t "CST"))

(check-true (time-range? r))
(check-false (time-range? "r"))

(check-report)
