(import (liii check)
        (liii date-time date)
        (liii date-time moment))

(check-set-mode! 'report-failed)

;; moment?
;; 判断给定对象是否为 moment 时刻对象。
;;
;; 语法
;; ----
;; (moment? obj)
;;
;; 参数
;; ----
;; obj : any
;;
;; 返回值
;; ----
;; boolean? 若为 moment 返回 #t，否则返回 #f。
;;
;; 示例
;; ----
;; (moment? (make-moment (make-date 2025 1 1) 0)) => #t

(check-true (moment? (make-moment (make-date 2025 1 1) 0)))
(check-true (moment? (make-moment (make-date 2020 2 29) 3600)))
(check-false (moment? (make-date 2025 1 1)))
(check-false (moment? 100))
(check-false (moment? #f))
(check-false (moment? "moment"))

(check-report)
