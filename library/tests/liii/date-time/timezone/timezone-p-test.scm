(import (liii check)
        (liii date-time timezone))

(check-set-mode! 'report-failed)

;; timezone?
;; 判断给定对象是否为时区对象。
;;
;; 语法
;; ----
;; (timezone? obj)
;;
;; 参数
;; ----
;; obj : any
;;
;; 返回值
;; ----
;; boolean? 若为 timezone 返回 #t，否则返回 #f。
;;
;; 示例
;; ----
;; (timezone? (utc-timezone)) => #t

(check-true (timezone? (utc-timezone)))
(check-true (timezone? (system-timezone)))
(check-false (timezone? "UTC"))
(check-false (timezone? #f))
(check-false (timezone? 0))

(check-report)
