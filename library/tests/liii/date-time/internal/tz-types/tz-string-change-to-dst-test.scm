;; tz-string-change-to-dst
;; 读取 POSIX TZ 记录的进入夏令时转移字段。
;;
;; 语法
;; ----
;; tz-string-change-to-dst ts
;;
;; 参数
;; ----
;; ts : tz-string? POSIX TZ 定义记录。
;;
;; 返回值
;; ----
;; tz-transition? 或 #f 进入夏令时的转移。

(import (liii check) (liii date-time internal tz-types))

(check-set-mode! 'report-failed)

(define tr (tz-transition (tz-julian-without-leap-day 60) (tz-offset #f 2 0 0)))

(define ts
  (tz-string "EST" (tz-offset #f 5 0 0) "EDT" (tz-offset #f 4 0 0) tr #f)
) ;define

(check (tz-transition? (tz-string-change-to-dst ts)) => #t)
(check (tz-string-change-to-dst (tz-string "EST" (tz-offset #f 5 0 0) "EDT" (tz-offset #f 4 0 0) #f #f)
       ) ;tz-string-change-to-dst
  =>
  #f
) ;check

(check-report)
