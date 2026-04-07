(import (liii check)
        (liii error)
        (liii set)
        (srfi srfi-128)
) ;import

(check-set-mode! 'report-failed)

;; set-intersection
;; 返回多个 set 的交集。
;;
;; 语法
;; ----
;; (set-intersection set1 set2 ...)
;;
;; 参数
;; ----
;; set1, set2 ... : set
;; 参与交集的 set。
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
;; (set-intersection (set 1 2 3) (set 2 3 4)) => 包含 2, 3 的 set

(define s-empty (set))
(define s-1 (set 1))
(define s-1-2-3 (set 1 2 3))
(define s-2-3-4 (set 2 3 4))

;; Test basic intersection
(define s-inter-1 (set-intersection s-1-2-3 s-2-3-4))
(check (set-size s-inter-1) => 2)
(check-true (set-contains? s-inter-1 2))
(check-true (set-contains? s-inter-1 3))

;; Test multiple sets intersection
(define s-inter-2 (set-intersection s-1-2-3 s-2-3-4 (set 2 3)))
(check (set-size s-inter-2) => 2)
(check-true (set-contains? s-inter-2 2))
(check-true (set-contains? s-inter-2 3))

;; Test element source (using case-insensitive comparator)
(define string-ci-comparator
  (make-comparator string? string-ci=? string-ci<?
    (lambda (s) (string-hash (string-map char-downcase s)))
  ) ;make-comparator
) ;define
(define s-inter-ci-1 (list->set-with-comparator string-ci-comparator '("Apple" "Banana")))
(define s-inter-ci-2 (list->set-with-comparator string-ci-comparator '("apple" "Pear")))
(define s-inter-ci (set-intersection s-inter-ci-1 s-inter-ci-2))
(check (set-member s-inter-ci "apple" 'not-found) => "Apple")
(check (set-size s-inter-ci) => 1)

;; Test type and comparator errors
(check-catch 'type-error (set-intersection "not a set" s-1))
(define s-str-ci (list->set-with-comparator string-ci-comparator '("test")))
(check-catch 'value-error (set-intersection s-1 s-str-ci))

(check-report)
