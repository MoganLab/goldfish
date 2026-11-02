;; dotted-list?
;; 判断一个对象是否为dotted list。
;;
;; 语法
;; ----
;; (dotted-list? obj)
;;
;; 参数
;; ----
;; obj : any
;; 任意对象
;;
;; 返回值
;; ------
;; boolean?
;; 如果obj是dotted list返回#t，否则返回#f。

(import (srfi srfi-1) (liii check))

(check-set-mode! 'report-failed)

(check-true (dotted-list? 1))
(check-true (dotted-list? '(1 . 2)))
(check-true (dotted-list? '(1 2 . 3)))

(check-false (dotted-list? (circular-list 1 2 3)))
(check-false (dotted-list? '()))
(check-false (dotted-list? '(a)))
(check-false (dotted-list? '(a b)))
(check-false (dotted-list? '(a b (c d))))

(check-true (dotted-list? 'a))
(check-true (dotted-list? 'symbol))
(check-true (dotted-list? "string"))
(check-true (dotted-list? 42))
(check-true (dotted-list? '(a . b)))
(check-true (dotted-list? '(a b . c)))
(check-true (dotted-list? '(a b c . d)))
(check-true (dotted-list? '(a b c d . e)))
(check-true (dotted-list? '(() . a)))
(check-true (dotted-list? '(a () . b)))
(check-true (dotted-list? '(a b () . c)))

(check-true (dotted-list? '((a . b) c . d)))
(check-true (dotted-list? '(a (b . c) . d)))
(check-true (dotted-list? '(a b . c)))

(check-false (dotted-list? '(())))
(check-false (dotted-list? '(a ())))
(check-false (dotted-list? '(() a b)))
(check-true (dotted-list? '(a () . b)))

(check-true (dotted-list? '(a b c . d)))
(check-false (dotted-list? '(a b (c . d))))

(check-false (dotted-list? '()))
(check-false (dotted-list? '(a)))
(check-false (dotted-list? '(a b)))
(check-false (dotted-list? '(a b c)))
(check-true (dotted-list? '(a . b)))
(check-true (dotted-list? '(a b . c)))
(check-true (dotted-list? 'a))

(let ((lst (list 1 2 3)))
  (set-cdr! (last-pair lst) lst)
  (check-false (dotted-list? lst)))

(check-true (dotted-list? '(a (b . c) . d)))
(check-false (dotted-list? '((a b) (c d))))
(check-true (dotted-list? '((a b) . c)))

(check-report)
