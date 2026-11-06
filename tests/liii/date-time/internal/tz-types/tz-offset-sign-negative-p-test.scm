;; tz-offset-sign-negative?
;; 读取偏移记录的负号标志字段。
;;
;; 语法
;; ----
;; tz-offset-sign-negative? off
;;
;; 参数
;; ----
;; off : tz-offset? 偏移记录。
;;
;; 返回值
;; ----
;; boolean? 为负偏移时返回 #t。

(import (liii check) (liii date-time internal tz-types))

(check-set-mode! 'report-failed)

(define off (tz-offset #t 5 0 0))

(check (tz-offset-sign-negative? off) => #t)
(check (tz-offset-sign-negative? (tz-offset #f 5 0 0)) => #f)

(check-report)
