(import (liii check)
        (liii error)
        (liii set)
        (srfi srfi-128)
) ;import

(check-set-mode! 'report-failed)

;; set-difference!
;; 就地更新 set1，使其成为与其余 set 的差集。
;;
;; 语法
;; ----
;; (set-difference! set1 set2 ...)
;;
;; 参数
;; ----
;; set1, set2 ... : set
;; 参与差集的 set。
;;
;; 返回值
;; ------
;; set
;; 返回修改后的 set1，元素来自第一个 set。
;;
;; 注意
;; ----
;; 此函数会修改 set1。
;; 所有 set 的比较器必须相同。
;;
;; 示例
;; ----
;; (set-difference! (set 1 2 3 4) (set 2 3 4)) => 修改后的 set，仅包含 1

(define s-2-3-4 (set 2 3 4))
(define s-4-5 (set 4 5))

;; Test basic difference!
(define s-diff!-1 (set 1 2 3 4))
(define s-diff!-result (set-difference! s-diff!-1 s-2-3-4))
(check-true (eq? s-diff!-result s-diff!-1))
(check (set-size s-diff!-1) => 1)
(check-true (set-contains? s-diff!-1 1))
(check-false (set-contains? s-diff!-1 2))

;; Test multiple sets difference
(define s-diff!-2 (set 1 2 3 4 5))
(set-difference! s-diff!-2 s-2-3-4 s-4-5)
(check (set-size s-diff!-2) => 1)
(check-true (set-contains? s-diff!-2 1))

;; Test element source (using case-insensitive comparator)
(define string-ci-comparator
  (make-comparator string? string-ci=? string-ci<?
    (lambda (s) (string-hash (string-map char-downcase s)))))
(define s-diff!-ci-1 (list->set-with-comparator string-ci-comparator '("Apple" "Banana")))
(define s-diff!-ci-2 (list->set-with-comparator string-ci-comparator '("apple")))
(set-difference! s-diff!-ci-1 s-diff!-ci-2)
(check (set-size s-diff!-ci-1) => 1)
(check (set-member s-diff!-ci-1 "banana" 'not-found) => "Banana")

;; Test type and comparator errors
(check-catch 'type-error (set-difference! "not a set" (set 1)))
(define s-str-ci (list->set-with-comparator string-ci-comparator '("test")))
(define s-num (set 1))
(check-catch 'value-error (set-difference! s-num s-str-ci))

(check-report)
