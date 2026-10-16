(import (liii check)
        (liii date-time date))

(check-set-mode! 'report-failed)

;; date?
;; 判断给定对象是否为 (liii date-time date) 的日期对象。
;;
;; 语法
;; ----
;; (date? obj)
;;
;; 参数
;; ----
;; obj : any
;;
;; 返回值
;; ----
;; boolean? 如果是日期对象返回 #t，否则返回 #f。
;;
;; 示例
;; ----
;; (date? (make-date 2020 1 1)) => #t
;; (date? "2020-01-01") => #f

(check-true (date? (make-date 2020 1 1)))
(check-true (date? (make-date -2020 12 31)))
(check-false (date? 20200101))
(check-false (date? "2020-01-01"))
(check-false (date? 'date))
(check-false (date? #f))
(check-false (date? '(2020 1 1)))

(check-report)
