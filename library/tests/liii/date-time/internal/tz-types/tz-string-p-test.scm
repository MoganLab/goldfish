;; tz-string?
;; 判断对象是否为 POSIX TZ 定义记录。
;;
;; 语法
;; ----
;; tz-string? obj
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

(define ts
  (tz-string "EST" (tz-offset #f 5 0 0) "EDT" (tz-offset #f 4 0 0) #f #f)
) ;define

(check-true (tz-string? ts))
(check-false (tz-string? "EST"))

(check-report)
