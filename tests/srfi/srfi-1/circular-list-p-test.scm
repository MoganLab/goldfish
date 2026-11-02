;; circular-list?
;; 判断给定对象是否为循环列表。
;;
;; 语法
;; ----
;; (circular-list? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待检查的对象。
;;
;; 返回值
;; ------
;; boolean
;; 如果obj是循环列表则返回#t，否则返回#f。

(import (srfi srfi-1) (liii check))

(check-set-mode! 'report-failed)

(check-true (circular-list? (circular-list 1)))
(check-true (circular-list? (circular-list 1 2)))
(check-true (circular-list? (circular-list 1 2 3)))

(let* ((l (list 1 2 3))
       (end (last-pair l)))
  (set-cdr! end (cdr l))
  (check-true (circular-list? l)))

(check-false (circular-list? '()))
(check-false (circular-list? (list 1 2)))
(check-false (circular-list? (cons 1 2)))
(check-false (circular-list? '(1 2 3 4)))
(check-false (circular-list? 42))
(check-false (circular-list? "string"))

(check-catch 'wrong-number-of-args (circular-list?))
(check-catch 'wrong-number-of-args (circular-list? '(1 2) '(3 4)))

(check-report)
