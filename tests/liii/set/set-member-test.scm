(import (liii check)
  (liii error)
  (liii set)
  (srfi srfi-128)
) ;import


(check-set-mode! 'report-failed)


;; set-member
;; 查找 set 中与指定元素相等的元素。
;;
;; 语法
;; ----
;; (set-member set element default)
;;
;; 参数
;; ----
;; set : set
;; 要检查的 set。
;;
;; element : any
;; 要查找的元素。
;;
;; default : any
;; 如果 element 不在 set 中，返回的值。
;;
;; 返回值
;; ------
;; any
;; 如果 element 在 set 中，返回 set 中存储的那个元素（可能与 element 并不是同一个对象，但比较结果相等）。
;; 如果 element 不在 set 中，返回 default。
;;
;; 示例
;; ----
;; (set-member (set 1) 1 'not-found) => 1
;; (set-member (set 1) 2 'not-found) => 'not-found
;;
;; 错误处理
;; ----
;; type-error
;; 当 set 参数不是 set 时抛出。


(define s-empty (set))
(define s-1 (set 1))


(check (set-member s-1 1 'not-found)
  =>
  1
) ;check
(check (set-member s-1 2 'not-found)
  =>
  'not-found
) ;check
(check (set-member s-empty 1 'not-found)
  =>
  'not-found
) ;check


;; Test case where comparator considers elements equal but they are not eq?
;; Construct a case-insensitive string set
(define (my-string-ci-hash s)
  (string-hash (string-map char-downcase s)
  ) ;string-hash
) ;define


(define string-ci-comparator
  (make-comparator string?
    string-ci=?
    string-ci<?
    my-string-ci-hash
  ) ;make-comparator
) ;define
(define s-str-ci
  (list->set-with-comparator string-ci-comparator
    '("Apple" "Banana")
  ) ;list->set-with-comparator
) ;define


(check (set-contains? s-str-ci "apple")
  =>
  #t
) ;check
;; set-member should return "Apple" (stored in set), not "apple" (the query)
(check (set-member s-str-ci "apple" 'not-found)
  =>
  "Apple"
) ;check
(check (set-member s-str-ci
         "banana"
         'not-found
       ) ;set-member
  =>
  "Banana"
) ;check
(check (set-member s-str-ci "pear" 'not-found)
  =>
  'not-found
) ;check


(check-catch 'type-error
  (set-member "not a set" 1 'default)
) ;check-catch


(check-report)
