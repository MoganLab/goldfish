(import (liii check)
        (liii error)
        (liii set)
        (srfi srfi-128)
) ;import

(check-set-mode! 'report-failed)

;; set-intersection!
;; 就地更新 set1，使其成为多个 set 的交集。
;;
;; 语法
;; ----
;; (set-intersection! set1 set2 ...)
;;
;; 参数
;; ----
;; set1, set2 ... : set
;; 参与交集的 set。
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
;; (set-intersection! (set 1 2 3 4) (set 2 3 4)) => 修改后的 set，包含 2, 3, 4

(define s-2-3-4 (set 2 3 4))

;; Test basic intersection!
(define s-inter!-1 (set 1 2 3 4))
(define s-inter!-result (set-intersection! s-inter!-1 s-2-3-4))
(check-true (eq? s-inter!-result s-inter!-1))
(check (set-size s-inter!-1) => 3)
(check-true (set-contains? s-inter!-1 2))
(check-true (set-contains? s-inter!-1 3))
(check-true (set-contains? s-inter!-1 4))

;; Test multiple sets intersection
(define s-inter!-2 (set 1 2 3 4))
(set-intersection! s-inter!-2 s-2-3-4 (set 2 3))
(check (set-size s-inter!-2) => 2)
(check-true (set-contains? s-inter!-2 2))
(check-true (set-contains? s-inter!-2 3))

;; Test element source (using case-insensitive comparator)
(define string-ci-comparator
  (make-comparator string? string-ci=? string-ci<?
    (lambda (s) (string-hash (string-map char-downcase s)))))
(define s-inter!-ci-1 (list->set-with-comparator string-ci-comparator '("Apple" "Banana")))
(define s-inter!-ci-2 (list->set-with-comparator string-ci-comparator '("apple" "Pear")))
(set-intersection! s-inter!-ci-1 s-inter!-ci-2)
(check (set-member s-inter!-ci-1 "apple" 'not-found) => "Apple")
(check (set-size s-inter!-ci-1) => 1)

;; Test type and comparator errors
(check-catch 'type-error (set-intersection! "not a set" (set 1)))
(define s-str-ci (list->set-with-comparator string-ci-comparator '("test")))
(define s-num (set 1))
(check-catch 'value-error (set-intersection! s-num s-str-ci))

(check-report)
