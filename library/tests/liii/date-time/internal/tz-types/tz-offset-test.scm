;; tz-offset
;; 构造 POSIX 时区偏移记录。
;;
;; 语法
;; ----
;; tz-offset sign? hour minute second
;;
;; 参数
;; ----
;; sign? : boolean? 是否为负偏移。
;; hour : integer? 小时。
;; minute : integer? 分钟。
;; second : integer? 秒。
;;
;; 返回值
;; ----
;; tz-offset? 新建的偏移记录。

(import (liii check) (liii date-time internal tz-types))

(check-set-mode! 'report-failed)

(define off (tz-offset #f 5 30 0))

(check-true (tz-offset? off))

(check-report)
