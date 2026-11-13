;; set->bag
;; 将 set 转换为每个元素出现次数为 1 的 bag。
;;
;; 语法
;; ----
;; (set->bag set)
;;
;; 参数
;; ----
;; set : set
;; 源 set。
;;
;; 返回值
;; ----
;; bag
;; 返回包含 set 中所有元素的新 bag，使用与 set 相同的比较器。
;;
;; 错误处理
;; ----
;; type-error
;; 当参数不是 set 时抛出 type-error。

(import (liii check) (liii error) (liii set) (liii bag) (srfi srfi-128))

(check-set-mode! 'report-failed)

(define s-empty (set))
(define b-from-empty (set->bag s-empty))
(check-true (bag? b-from-empty))
(check-true (bag-empty? b-from-empty))
(check-true (eq? (bag-comparator b-from-empty) (set-element-comparator s-empty)))

(define s-123 (set 1 2 3))
(define b-123 (set->bag s-123))
(check-true (bag? b-123))
(check (bag-size b-123) => 3)
(check-true (bag-contains? b-123 1))
(check-true (bag-contains? b-123 2))
(check-true (bag-contains? b-123 3))
(check (bag-count (lambda (x) (= x 2)) b-123) => 1)

(define eq-comp (make-eq-comparator))
(define s-sym (make-set-with-comparator eq-comp 'x 'y))
(define b-sym (set->bag s-sym))
(check-true (eq? (bag-comparator b-sym) eq-comp))
(check (bag-size b-sym) => 2)
(check-true (bag-contains? b-sym 'x))
(check-true (bag-contains? b-sym 'y))

(check-catch 'type-error (set->bag "not a set"))

(check-report)
