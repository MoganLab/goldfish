;; tz-offset?
;; 判断对象是否为 POSIX 时区偏移记录。
;;
;; 语法
;; ----
;; (tz-offset? obj)
;;
;; 参数
;; ----
;; obj : any? 任意对象。
;;
;; 返回值
;; ----
;; boolean? 是偏移记录返回 #t。

(import (liii check) (liii date-time internal tz-types))

(check-set-mode! 'report-failed)

(check-true (tz-offset? (tz-offset #f 5 30 0)))
(check-false (tz-offset? "offset"))
(check-false (tz-offset? 5))

(check-report)
