;; tz-string-change-to-std
;; 读取 POSIX TZ 记录的退出夏令时转移字段。
;;
;; 语法
;; ----
;; tz-string-change-to-std ts
;;
;; 参数
;; ----
;; ts : tz-string? POSIX TZ 定义记录。
;;
;; 返回值
;; ----
;; tz-transition? 或 #f 退出夏令时的转移。

(import (liii check) (liii date-time internal tz-types))

(check-set-mode! 'report-failed)

(define tr
  (tz-transition (tz-julian-without-leap-day 274) (tz-offset #f 2 0 0))
) ;define

(define ts
  (tz-string "EST" (tz-offset #f 5 0 0) "EDT" (tz-offset #f 4 0 0) #f tr)
) ;define

(check (tz-transition? (tz-string-change-to-std ts)) => #t)
(check (tz-string-change-to-std (tz-string "EST" (tz-offset #f 5 0 0) "EDT" (tz-offset #f 4 0 0) #f #f)
       ) ;tz-string-change-to-std
  =>
  #f
) ;check

(check-report)
