(import (liii check)
        (liii error)
        (liii set)
        (srfi srfi-128)
) ;import

(check-set-mode! 'report-failed)

;; set-xor!
;; 就地更新 set1，使其成为与 set2 的对称差集。
;;
;; 语法
;; ----
;; (set-xor! set1 set2)
;;
;; 参数
;; ----
;; set1, set2 : set
;; 参与对称差集的 set。
;;
;; 返回值
;; ------
;; set
;; 返回修改后的 set1。
;;
;; 注意
;; ----
;; 此函数会修改 set1。
;; 所有 set 的比较器必须相同。
;;
;; 示例
;; ----
;; (set-xor! (set 1 2 3) (set 2 3 4)) => 修改后的 set，包含 1, 4

(define s-2-3-4 (set 2 3 4))

;; Test basic xor!
(define s-xor!-1 (set 1 2 3))
(define s-xor!-result (set-xor! s-xor!-1 s-2-3-4))
(check-true (eq? s-xor!-result s-xor!-1))
(check (set-size s-xor!-1) => 2)
(check-true (set-contains? s-xor!-1 1))
(check-true (set-contains? s-xor!-1 4))

;; Test element source (using case-insensitive comparator)
(define string-ci-comparator
  (make-comparator string? string-ci=? string-ci<?
    (lambda (s) (string-hash (string-map char-downcase s)))
  ) ;make-comparator
) ;define
(define s-xor!-ci-1 (list->set-with-comparator string-ci-comparator '("Apple")))
(define s-xor!-ci-2 (list->set-with-comparator string-ci-comparator '("apple" "Banana")))
(set-xor! s-xor!-ci-1 s-xor!-ci-2)
(check (set-size s-xor!-ci-1) => 1)
(check (set-member s-xor!-ci-1 "banana" 'not-found) => "Banana")
(check-false (set-contains? s-xor!-ci-1 "apple"))

;; Test type and comparator errors
(check-catch 'type-error (set-xor! "not a set" (set 1)))
(define s-str-ci (list->set-with-comparator string-ci-comparator '("test")))
(define s-num (set 1))
(check-catch 'value-error (set-xor! s-num s-str-ci))

(check-report)
