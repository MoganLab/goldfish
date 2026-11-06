;; tz-string-stdoffset
;; 读取 POSIX TZ 记录的标准时间偏移字段。
;;
;; 语法
;; ----
;; tz-string-stdoffset ts
;;
;; 参数
;; ----
;; ts : tz-string? POSIX TZ 定义记录。
;;
;; 返回值
;; ----
;; tz-offset? 标准时间偏移。

(import (liii check) (liii date-time internal tz-types))

(check-set-mode! 'report-failed)

(define ts
  (tz-string "EST" (tz-offset #f 5 0 0) "EDT" (tz-offset #f 4 0 0) #f #f)
) ;define

(check (tz-offset-hour (tz-string-stdoffset ts)) => 5)

(check-report)
