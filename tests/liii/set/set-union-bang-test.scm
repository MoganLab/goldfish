(import (liii check)
        (liii error)
        (liii set)
        (srfi srfi-128)
) ;import

(check-set-mode! 'report-failed)

;; set-union!
;; 将多个 set 并入 set1（可变操作）。
;;
;; 语法
;; ----
;; (set-union! set1 set2 ...)
;;
;; 参数
;; ----
;; set1, set2 ... : set
;; 参与并集的 set。
;;
;; 返回值
;; ------
;; set
;; 返回修改后的 set1，元素来自它们首次出现的 set。
;;
;; 注意
;; ----
;; 此函数会修改 set1。
;; 所有 set 的比较器必须相同。
;;
;; 示例
;; ----
;; (set-union! (set 1 2 3) (set 2 3 4) (set 4 5)) => 修改后的 set，包含 1, 2, 3, 4, 5

(define s-2-3-4 (set 2 3 4))
(define s-4-5 (set 4 5))

;; Test basic union!
(define s-union!-1 (set 1 2 3))
(define s-union!-result (set-union! s-union!-1 s-2-3-4 s-4-5))
(check-true (eq? s-union!-result s-union!-1))
(check (set-size s-union!-1) => 5)
(check-true (set-contains? s-union!-1 1))
(check-true (set-contains? s-union!-1 5))

;; Test element source (using case-insensitive comparator)
(define string-ci-comparator
  (make-comparator string? string-ci=? string-ci<?
    (lambda (s) (string-hash (string-map char-downcase s)))
  ) ;make-comparator
) ;define
(define s-union!-ci-1 (list->set-with-comparator string-ci-comparator '("Apple")))
(define s-union!-ci-2 (list->set-with-comparator string-ci-comparator '("apple" "Banana")))
(define s-union!-ci (set-union! s-union!-ci-1 s-union!-ci-2))
(check-true (eq? s-union!-ci s-union!-ci-1))
(check (set-member s-union!-ci "apple" 'not-found) => "Apple")
(check (set-member s-union!-ci "banana" 'not-found) => "Banana")

;; Test type and comparator errors
(check-catch 'type-error (set-union! "not a set" (set 1)))
(define s-str-ci (list->set-with-comparator string-ci-comparator '("test")))
(define s-num (set 1))
(check-catch 'value-error (set-union! s-num s-str-ci))

(check-report)
