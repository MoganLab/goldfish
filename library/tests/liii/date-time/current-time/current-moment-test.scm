;; current-moment
;; 获取系统当前的物理时刻（moment）。
;;
;; 语法
;; ----
;; (current-moment)
;;
;; 参数
;; ----
;; 无。
;;
;; 返回值
;; ----
;; moment?
;;

(import (liii check) (liii date-time moment) (liii date-time current-time))

(check-set-mode! 'report-failed)

(check-true (moment? (current-moment)))

(check-report)
