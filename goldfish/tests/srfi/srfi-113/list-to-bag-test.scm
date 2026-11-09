;; list->bag
;; 使用指定比较器将列表转换为 bag。
;;
;; 语法
;; ----
;; (list->bag comparator list)
;;
;; 参数
;; ----
;; comparator : comparator
;; 用于判定元素等价性的比较器。
;;
;; list : list
;; 要转换的列表。
;;
;; 返回值
;; ----
;; bag
;; 返回包含列表中所有元素、并使用 comparator 作为比较器的 bag。
;;
;; 注意
;; ----
;; 列表中的重复元素会被保留。
;;
;; 错误处理
;; ----
;; 无异常抛出

(import (liii check) (srfi srfi-113) (srfi srfi-128))

(check-set-mode! 'report-failed)

(define comp (make-default-comparator))


(define b-empty (bag comp))

(define b-1-2-3 (bag comp 1 2 3))


(define b-list-1 (list->bag comp '(1 2 3)))

(check-true (bag? b-list-1))
(check-true (eq? (bag-comparator b-list-1) comp))
(check (bag-size b-list-1) => 3)
(check-false (eq? b-1-2-3 b-list-1))


(define b-list-empty (list->bag comp '()))

(check-true (bag-empty? b-list-empty))
(check (bag-size b-list-empty) => 0)


;; 列表中的重复元素会被保留

(define b-list-dup (list->bag comp '(1 2 2 1)))

(check (bag-size b-list-dup) => 4)
(check (bag-count (lambda (x) (= x 2)) b-list-dup) => 2)

(check-report)
