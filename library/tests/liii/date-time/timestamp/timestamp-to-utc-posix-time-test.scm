(import (liii check)
        (liii date-time timezone)
        (liii date-time timestamp))

(check-set-mode! 'report-failed)

;; timestamp->utc-posix-time
;; 将时间戳转换为对应的 UTC POSIX 秒数。
;;
;; 语法
;; ----
;; (timestamp->utc-posix-time ts)
;;
;; 参数
;; ----
;; ts : timestamp?
;;
;; 返回值
;; ----
;; rational?
;;
;; 示例
;; ----
;; (timestamp->utc-posix-time (make-timestamp 1970 1 1 0 0 0 (utc-timezone))) => 0

(check (timestamp->utc-posix-time (make-timestamp 1970 1 1 0 0 0 (utc-timezone))) => 0)

(let ((t1 (make-timestamp 2015 6 30 23 59 59 (utc-timezone)))
      (t2 (make-timestamp 2015 6 30 23 59 60 (utc-timezone)))
      (t3 (make-timestamp 2015 7 1 2 59 59 (tz-timezone "Europe/Vilnius"))))
  (check (timestamp->utc-posix-time t1) => 1435708799)
  (check (timestamp->utc-posix-time t2) => 1435708799)
  (check (timestamp->utc-posix-time t3) => 1435708799))

(check-catch 'type-error (timestamp->utc-posix-time #f))

(check-report)
