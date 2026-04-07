(import (liii check)
        (liii error)
        (liii set)
        (srfi srfi-128)
) ;import

(check-set-mode! 'report-failed)

;; set-map
;; 对 set 中的每个元素应用 proc，并返回一个新 set。
;;
;; 语法
;; ----
;; (set-map comparator proc set)
;;
;; 参数
;; ----
;; comparator : comparator
;; 结果 set 的比较器。
;;
;; proc : procedure
;; 映射函数。
;;
;; set : set
;; 源 set。
;;
;; 返回值
;; ------
;; set
;; 返回新的 set，其中元素为 proc 的映射结果。
;;
;; 注意
;; ----
;; 如果映射后出现等价元素（基于 comparator），重复元素会被去重。
;;
;; 示例
;; ----
;; (set-map comp (lambda (x) (+ x 10)) (set 1 2 3)) => 包含 11, 12, 13 的 set

(define s-empty (set))
(define comp (set-element-comparator s-empty))
(define string-ci-comparator
  (make-comparator string? string-ci=? string-ci<?
    (lambda (s) (string-hash (string-map char-downcase s)))
  ) ;make-comparator
) ;define

;; Test basic mapping
(define s-map-1 (set 1 2 3))
(define s-map-2 (set-map comp (lambda (x) (+ x 10)) s-map-1))
(check-true (set? s-map-2))
(check-true (eq? (set-element-comparator s-map-2) comp))
(check (set-size s-map-2) => 3)
(check-true (set-contains? s-map-2 11))
(check-true (set-contains? s-map-2 12))
(check-true (set-contains? s-map-2 13))
(check-true (set=? s-map-1 (set 1 2 3))) ; Original set unchanged

;; Test deduplication behavior
(define s-map-dup (set 1 2 3 4 5))
(define s-map-dup-result (set-map comp (lambda (x) (quotient x 2)) s-map-dup))
(check (set-size s-map-dup-result) => 3)
(check-true (set-contains? s-map-dup-result 0))
(check-true (set-contains? s-map-dup-result 1))
(check-true (set-contains? s-map-dup-result 2))

;; Test with different comparator
(define s-map-sym (list->set-with-comparator (make-eq-comparator) '(foo bar baz)))
(define s-map-str (set-map string-ci-comparator symbol->string s-map-sym))
(check (set-member s-map-str "FOO" 'not-found) => "foo")
(check (set-member s-map-str "BAR" 'not-found) => "bar")
(check (set-member s-map-str "BAZ" 'not-found) => "baz")

(check-catch 'type-error (set-map comp (lambda (x) x) "not a set"))

(check-report)
