;; iset
;; 创建一个新的整数集合。
;;
;; 语法
;; ----
;; (iset element ...)
;;
;; 参数
;; ----
;; element ... : exact-integer
;; 初始元素（可选），元素必须为精确整数。
;;
;; 返回值
;; ----
;; iset
;; 返回包含给定元素的新 iset。

(import (liii check) (liii list) (srfi srfi-217))


(check-set-mode! 'report-failed)


;; ;; Utility


(define (init xs)
  (if (null? (cdr xs)) '() (cons (car xs) (init (cdr xs))))
) ;define


(define (constantly x)
  (lambda (_) x)
) ;define


(define pos-seq (iota 20 100 3))

(define neg-seq (iota 20 -100 3))

(define mixed-seq (iota 20 -10 3))

(define sparse-seq (iota 20 -10000 1003))


(define pos-set (list->iset pos-seq))

(define pos-set+ (iset-adjoin pos-set 9))

(define neg-set (list->iset neg-seq))

(define mixed-set (list->iset mixed-seq))

(define dense-set (make-range-iset 0 49))

(define sparse-set (list->iset sparse-seq))


(define all-test-sets (list pos-set neg-set mixed-set dense-set sparse-set))


;; 构造：空集合、单元素与多元素

(check-true (iset? (iset)))
(check (iset-size (iset)) => 0)
(check (iset->list (iset)) => '())

(check-true (iset? (iset 1)))
(check (iset->list (iset 1)) => '(1))

(check (iset-size (iset 1 2 3)) => 3)
(check (iset->list (iset 3 2 1)) => '(1 2 3))
(check (iset->list (iset 2 3 5 7 11)) => '(2 3 5 7 11))

;; 重复元素只保留一份
(check (iset-size (iset 1 2 2 3 3 3)) => 3)
(check (iset->list (iset 1 2 2 3 3 3)) => '(1 2 3))

;; 与其它构造方式结果一致
(check (iset=? (iset 1 2 3) (list->iset '(1 2 3))) => #t)
(check (iset=? (iset 1 2 3) (make-range-iset 1 4)) => #t)

;; 共享测试数据集均为合法 iset
(check-true (every iset? all-test-sets))
(check (iset-size pos-set) => 20)
(check (iset-size dense-set) => 49)
(check (iset-size (iset-adjoin pos-set 9)) => 21)


(check-report)
