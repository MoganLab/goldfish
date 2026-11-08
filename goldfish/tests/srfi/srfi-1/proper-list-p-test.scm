;; proper-list?
;; 判断一个对象是否为proper list。
;;
;; 语法
;; ----
;; (proper-list? obj)
;;
;; 参数
;; ----
;; obj : any
;; 任意对象
;;
;; 返回值
;; ------
;; boolean?
;; 如果obj是proper list返回#t，否则返回#f。

(import (srfi srfi-1) (liii check))

(check-set-mode! 'report-failed)

(check-true (proper-list? (list 1 2)))
(check-true (proper-list? '()))
(check-true (proper-list? '(1 2 3)))

(check-false (proper-list? '(a . b)))
(check-false (proper-list? '(a b . c)))
(check-false (proper-list? (circular-list 1 2 3)))

(check-true (proper-list? '(() ())))
(check-true (proper-list? '(a)))
(check-false (proper-list? 1))
(check-false (proper-list? 'hello))
(check-false (proper-list? "hello"))

(check-false (proper-list? '(a b . c)))
(check-true (proper-list? '(a b (c d))))
(check-true (proper-list? '(() a b)))
(check-true (proper-list? '(a b ())))

(check-false (proper-list? '(a . b)))
(check-false (proper-list? '(a b . c)))
(check-false (proper-list? '(a b c . d)))

(let ((lst (list 1 2 3)))
  (set-cdr! (last-pair lst) lst)
  (check-false (proper-list? lst)))

(check-report)
