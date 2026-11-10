;; null-list?
;; 判断一个对象是否为空列表。
;;
;; 语法
;; ----
;; (null-list? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待检查的对象。
;;
;; 返回值
;; ------
;; boolean
;; 当 obj 是空列表时返回 #t，否则返回 #f。

(import (srfi srfi-1) (liii check))

(check-set-mode! 'report-failed)

(check (null-list? '()) => #t)
(check (null-list? '(1 . 2)) => #f)
(check (null-list? '(1 2)) => #f)

(check-catch 'type-error (null-list? 1))
(check-catch 'type-error (null-list? "str"))

(check-report)
