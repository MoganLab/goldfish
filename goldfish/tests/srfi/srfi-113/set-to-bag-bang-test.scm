;; set->bag!
;; 将 set 中的元素就地添加到 bag 中。
;;
;; 语法
;; ----
;; (set->bag! bag set)
;;
;; 参数
;; ----
;; bag : bag
;; 目标 bag。
;;
;; set : set
;; 源 set。
;;
;; 返回值
;; ----
;; bag
;; 返回更新后的 bag。
;;
;; 错误处理
;; ----
;; type-error
;; 当参数类型不正确时抛出 type-error。
;;
;; value-error
;; 当 bag 与 set 的比较器不同时抛出 value-error。

(import (liii check)
  (liii error)
  (liii set)
  (liii bag)
  (rename (only (srfi srfi-113) bag) (bag srfi-bag))
  (srfi srfi-128)
) ;import

(check-set-mode! 'report-failed)

(define comp (set-element-comparator (set)))
(define b (srfi-bag comp 1 2 2))
(define s (make-set-with-comparator comp 2 3))

(define res (set->bag! b s))
(check-true (eq? res b))
(check (bag-size b) => 5)
(check (bag-count (lambda (x) (= x 1)) b) => 1)
(check (bag-count (lambda (x) (= x 2)) b) => 3)
(check (bag-count (lambda (x) (= x 3)) b) => 1)

(define s-other (make-set-with-comparator (make-eq-comparator) 'a))
(check-catch 'value-error (set->bag! b s-other))
(check-catch 'type-error (set->bag! "not a bag" s))
(check-catch 'type-error (set->bag! b "not a set"))

(check-report)
