(import (liii check) (liii os))

(check-set-mode! 'report-failed)

;; os-time
;; 返回当前系统时间的秒数（自 Unix 纪元起）。
;;
;; 语法
;; ----
;; (os-time)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ------
;; real
;; 当前时间的秒级时间戳。
;;
;; 示例
;; ----
;; (> (os-time) 1700000000) => #t

(check (real? (os-time)) => #t)
(check (> (os-time) 1700000000) => #t)

(check-report)
