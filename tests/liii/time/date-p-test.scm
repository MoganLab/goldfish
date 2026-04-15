(import (liii check)
  (liii time)
  (srfi srfi-19)
) ;import


(check-set-mode! 'report-failed)


;; date?
;; 判断对象是否为日期对象。
;;
;; 语法
;; ----
;; (date? obj)
;;
;; 参数
;; ----
;; obj : any? 任意对象。
;;
;; 返回值
;; ----
;; boolean? 如果obj是日期对象则返回#t，否则返回#f。
;;
;; 示例
;; ----
;; (date? (make-date 0 0 0 0 1 1 1970 0)) => #t
;; (date? 123) => #f
;; (date? "string") => #f


;; Test date?
(check-true (date? (make-date 0 0 0 0 1 1 1970 0))
) ;check-true
(check-false (date? 123))
(check-false (date? "string"))
(check-false (date? 'symbol))
(check-false (date? #t))
(check-false (date? #(vector)))
(check-false (date? (cons 1 2)))
(check-false (date? (make-time TIME-UTC 0 0))
) ;check-false


(check-report)
