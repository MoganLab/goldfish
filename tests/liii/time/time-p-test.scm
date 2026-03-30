(import (liii check)
        (liii time)
        (srfi srfi-19)
) ;import

(check-set-mode! 'report-failed)

;; time?
;; 判断对象是否为时间对象。
;;
;; 语法
;; ----
;; (time? obj)
;;
;; 参数
;; ----
;; obj : any?
;; 任意对象。
;;
;; 返回值
;; ----
;; boolean?
;; 如果obj是时间对象则返回#t，否则返回#f。
;;
;; 示例
;; ----
;; (time? (make-time TIME-UTC 0 0)) => #t
;; (time? 123) => #f
;; (time? "string") => #f
;;
;; 错误处理
;; ----
;; 无

;; Test time?
(check-true  (time? (make-time TIME-UTC 0 0)))
(check-false (time? 123))
(check-false (time? "string"))
(check-false (time? 'symbol))
(check-false (time? #t))
(check-false (time? #(vector)))
(check-false (time? (cons 1 2)))

(check-report)
