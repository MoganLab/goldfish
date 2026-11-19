;; select-list?
;; 判断给定对象是否为选择列表组件。
;;
;; 语法
;; ----
;; (select-list? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待检查的对象。
;;
;; 返回值
;; ----
;; boolean?
;; 如果为选择列表组件则返回 #t，否则返回 #f。

(import (scheme base) (liii check) (liii tui select-list))

(check-set-mode! 'report-failed)

(check-true (select-list? (make-select-list '() 5)))
(check-false (select-list? "not-a-select-list"))
(check-false (select-list? 123))
(check-false (select-list? '()))

(check-report)
