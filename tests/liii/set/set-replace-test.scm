(import (liii check)
        (liii error)
        (liii set)
        (srfi srfi-128)
) ;import

(check-set-mode! 'report-failed)

;; set-replace
;; 返回一个新的 set，其中指定的元素被替换。
;;
;; 语法
;; ----
;; (set-replace set element)
;;
;; 参数
;; ----
;; set : set
;; 初始 set。
;;
;; element : any
;; 用来替换的元素。
;;
;; 返回值
;; ------
;; set
;; 返回一个新的 set。
;; 如果 set 中包含与 element 相等的元素（根据比较器），则该元素被 element 替换（对于 equals 但 not eq? 的情况很有用）。
;; 如果 set 中不包含 element，则返回原 set（不做任何修改）。
;;
;; 注意
;; ----
;; 此函数不修改原 set。
;;
;; 示例
;; ----
;; (set-replace (set 1) 1) => 新的 set，包含 1

(define s-1 (set 1))

;; Test basic replace
(define s-replace-1 (set-replace s-1 1))
(check (set-size s-replace-1) => 1)
(check-true (set-contains? s-replace-1 1))
(check-true (set=? s-replace-1 s-1)) ; Content same
(check-false (eq? s-replace-1 s-1)) ; But should be new set (since replacement logic occurred)

(define s-replace-2 (set-replace s-1 2))
(check-true (eq? s-replace-2 s-1)) ; 2 not in set, return original set

;; Test replacing equals but not eq? element
(define string-ci-comparator
  (make-comparator string? string-ci=? string-ci<?
    (lambda (s) (string-hash (string-map char-downcase s)))
  ) ;make-comparator
) ;define
(define s-str-ci-2 (list->set-with-comparator string-ci-comparator '("Apple" "Banana")))
(check (set-member s-str-ci-2 "apple" 'not-found) => "Apple")

(define s-replace-3 (set-replace s-str-ci-2 "apple"))
(check (set-member s-replace-3 "apple" 'not-found) => "apple") ; Should be replaced with "apple"
(check-false (eq? s-replace-3 s-str-ci-2)) ; Should be new set

;; Test type error
(check-catch 'type-error (set-replace "not a set" 1))

(check-report)
