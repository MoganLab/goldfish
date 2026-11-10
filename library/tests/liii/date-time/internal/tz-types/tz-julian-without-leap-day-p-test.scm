;; tz-julian-without-leap-day?
;; 判断对象是否为忽略闰日的儒略日记录。
;;
;; 语法
;; ----
;; tz-julian-without-leap-day? obj
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

(define jd (tz-julian-without-leap-day 60))

(check-true (tz-julian-without-leap-day? jd))
(check-false (tz-julian-without-leap-day? 60))

(check-report)
