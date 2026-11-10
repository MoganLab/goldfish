;; tz-transition?
;; 判断对象是否为 POSIX 转移记录。
;;
;; 语法
;; ----
;; tz-transition? obj
;;
;; 参数
;; ----
;; obj : any? 任意对象。
;;
;; 返回值
;; ----
;; boolean? 是该类记录返回 #t。

(import (liii check) (liii date-time internal tz-types))

(check-set-mode! 'report-failed)

(define tr (tz-transition (tz-julian-without-leap-day 60) (tz-offset #f 2 0 0)))

(check-true (tz-transition? tr))
(check-false (tz-transition? 0))

(check-report)
