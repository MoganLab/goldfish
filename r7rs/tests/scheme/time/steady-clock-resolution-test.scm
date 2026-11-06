;; steady-clock-resolution
;; 获取稳定时钟的分辨率。
;;
;; 语法
;; ----
;; steady-clock-resolution
;;
;; 参数
;; ----
;; 无参数。
;;
;; 返回值
;; ----
;; integer
;; 稳定时钟（steady_clock）的分辨率，以纳秒为单位。

(import (liii check) (scheme time))

(check-set-mode! 'report-failed)

(check (integer? steady-clock-resolution) => #t)
(check (> steady-clock-resolution 0) => #t)
(check (<= steady-clock-resolution 1000000000) => #t)

(check-report)
