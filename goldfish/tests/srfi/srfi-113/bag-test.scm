;; bag
;; 使用指定比较器创建一个新的 bag。
;;
;; 语法
;; ----
;; (bag comparator element ...)
;;
;; 参数
;; ----
;; comparator : comparator
;; 用于判定元素等价性的比较器。
;;
;; element ... : any
;; 初始元素。
;;
;; 返回值
;; ----
;; bag
;; 返回包含指定元素、并使用 comparator 作为比较器的 bag。
;;
;; 错误处理
;; ----
;; 无异常抛出

(import (liii check) (srfi srfi-113) (srfi srfi-128))

(check-set-mode! 'report-failed)

(define comp (make-default-comparator))


(define b-empty (bag comp))

(check-true (bag? b-empty))
(check-true (bag-empty? b-empty))


(define b-1-2 (bag comp 1 2))

(check-true (bag-contains? b-1-2 1))
(check-true (bag-contains? b-1-2 2))
(check-false (bag-contains? b-1-2 3))
(check (bag-size b-1-2) => 2)

;; 重复元素会被保留

(define b-dup (bag comp 1 2 2))

(check (bag-size b-dup) => 3)
(check (bag-count (lambda (x) (= x 2)) b-dup) => 2)

;; 比较器被保留

(check-true (eq? (bag-comparator b-1-2) comp))

(check-report)
