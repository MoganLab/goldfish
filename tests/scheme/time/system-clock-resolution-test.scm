;; system-clock-resolution
;; 获取系统时钟的分辨率。
;;
;; 语法
;; ----
;; system-clock-resolution
;;
;; 参数
;; ----
;; 无参数。
;;
;; 返回值
;; ----
;; integer
;; 系统时钟（system_clock）的分辨率，以纳秒为单位。

(import (liii check) (scheme time))

(check-set-mode! 'report-failed)

(check (integer? system-clock-resolution) => #t)
(check (> system-clock-resolution 0) => #t)
(check (<= system-clock-resolution 1000000000) => #t)

(check-report)
