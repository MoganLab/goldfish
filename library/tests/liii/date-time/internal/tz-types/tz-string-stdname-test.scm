;; tz-string-stdname
;; 读取 POSIX TZ 记录的标准时间缩写字段。
;;
;; 语法
;; ----
;; tz-string-stdname ts
;;
;; 参数
;; ----
;; ts : tz-string? POSIX TZ 定义记录。
;;
;; 返回值
;; ----
;; string? 标准时间缩写。

(import (liii check) (liii date-time internal tz-types))

(check-set-mode! 'report-failed)

(define ts
  (tz-string "EST" (tz-offset #f 5 0 0) "EDT" (tz-offset #f 4 0 0) #f #f)
) ;define

(check (tz-string-stdname ts) => "EST")

(check-report)
