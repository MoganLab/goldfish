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
(check-true (set-contains? s-replace-1 1)
) ;check-true
(check-true (set=? s-replace-1 s-1))
(check-false (eq? s-replace-1 s-1))


(define s-replace-2 (set-replace s-1 2))
(check-true (eq? s-replace-2 s-1))


;; Test replacing equals but not eq? element
(define string-ci-comparator
  (make-comparator string?
    string-ci=?
    string-ci<?
    (lambda (s)
      (string-hash (string-map char-downcase s)
      ) ;string-hash
    ) ;lambda
  ) ;make-comparator
) ;define
(define s-str-ci-2
  (list->set-with-comparator string-ci-comparator
    '("Apple" "Banana")
  ) ;list->set-with-comparator
) ;define
(check (set-member s-str-ci-2
         "apple"
         'not-found
       ) ;set-member
  =>
  "Apple"
) ;check


(define s-replace-3
  (set-replace s-str-ci-2 "apple")
) ;define
(check (set-member s-replace-3
         "apple"
         'not-found
       ) ;set-member
  =>
  "apple"
) ;check
(check-false (eq? s-replace-3 s-str-ci-2)
) ;check-false


;; Test type error
(check-catch 'type-error
  (set-replace "not a set" 1)
) ;check-catch


(check-report)
