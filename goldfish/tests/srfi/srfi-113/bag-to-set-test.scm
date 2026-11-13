;; bag->set
;; 将 bag 转换为包含其唯一元素的 set。
;;
;; 语法
;; ----
;; (bag->set bag)
;;
;; 参数
;; ----
;; bag : bag
;; 源 bag。
;;
;; 返回值
;; ----
;; set
;; 返回包含 bag 中所有去重元素的新 set，使用与 bag 相同的比较器。
;;
;; 错误处理
;; ----
;; type-error
;; 当参数不是 bag 时抛出 type-error。

(import (liii check)
  (liii error)
  (liii set)
  (liii bag)
  (rename (only (srfi srfi-113) bag) (bag srfi-bag))
  (srfi srfi-128)
) ;import

(check-set-mode! 'report-failed)

(define b-empty (bag))
(define s-from-empty (bag->set b-empty))
(check-true (set? s-from-empty))
(check-true (set-empty? s-from-empty))
(check-true (eq? (set-element-comparator s-from-empty) (bag-comparator b-empty)))

(define b-dup (bag 1 2 2 3 3 3))
(define s-from-dup (bag->set b-dup))
(check-true (set? s-from-dup))
(check (set-size s-from-dup) => 3)
(check-true (set-contains? s-from-dup 1))
(check-true (set-contains? s-from-dup 2))
(check-true (set-contains? s-from-dup 3))

(define eq-comp (make-eq-comparator))
(define b-sym (srfi-bag eq-comp 'a 'b 'a))
(define s-sym (bag->set b-sym))
(check-true (eq? (set-element-comparator s-sym) eq-comp))
(check (set-size s-sym) => 2)
(check-true (set-contains? s-sym 'a))
(check-true (set-contains? s-sym 'b))

(check-catch 'type-error (bag->set "not a bag"))

(check-report)
