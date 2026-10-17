(import (liii check)
        (liii date-time moment)
        (liii date-time current-time))

(check-set-mode! 'report-failed)

;; current-moment
;; 获取系统当前的物理时刻（moment）。
;;
;; 语法
;; ----
;; (current-moment)
;;
;; 返回值
;; ----
;; moment?
;;
;; 示例
;; ----
;; (current-moment)

(check-true (moment? (current-moment)))

(check-report)
