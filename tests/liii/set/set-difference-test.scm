(import (liii check)
        (liii error)
        (liii set)
        (srfi srfi-128)
) ;import

(check-set-mode! 'report-failed)

;; set-difference
;; 返回第一个 set 与其余 set 的差集。
;;
;; 语法
;; ----
;; (set-difference set1 set2 ...)
;;
;; 参数
;; ----
;; set1, set2 ... : set
;; 参与差集的 set。
;;
;; 返回值
;; ------
;; set
;; 返回新的 set，元素来自第一个 set。
;;
;; 注意
;; ----
;; 所有 set 的比较器必须相同。
;;
;; 示例
;; ----
;; (set-difference (set 1 2 3) (set 2 3 4)) => 包含 1 的 set

(define s-empty (set))
(define s-1 (set 1))
(define s-1-2-3 (set 1 2 3))
(define s-2-3-4 (set 2 3 4))
(define s-4-5 (set 4 5))

;; Test basic difference
(define s-diff-1 (set-difference s-1-2-3 s-2-3-4))
(check (set-size s-diff-1) => 1)
(check-true (set-contains? s-diff-1 1))
(check-false (set-contains? s-diff-1 2))
(check-false (set-contains? s-diff-1 3))

;; Test multiple sets difference
(define s-diff-2 (set-difference s-1-2-3 s-2-3-4 s-4-5))
(check (set-size s-diff-2) => 1)
(check-true (set-contains? s-diff-2 1))

;; Test element source (using case-insensitive comparator)
(define string-ci-comparator
  (make-comparator string? string-ci=? string-ci<?
    (lambda (s) (string-hash (string-map char-downcase s)))))
(define s-diff-ci-1 (list->set-with-comparator string-ci-comparator '("Apple" "Banana")))
(define s-diff-ci-2 (list->set-with-comparator string-ci-comparator '("apple")))
(define s-diff-ci (set-difference s-diff-ci-1 s-diff-ci-2))
(check (set-size s-diff-ci) => 1)
(check (set-member s-diff-ci "banana" 'not-found) => "Banana")

;; Test type and comparator errors
(check-catch 'type-error (set-difference "not a set" s-1))
(define s-str-ci (list->set-with-comparator string-ci-comparator '("test")))
(check-catch 'value-error (set-difference s-1 s-str-ci))

(check-report)
