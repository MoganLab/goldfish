(import (liii check)
  (liii error)
  (liii set)
  (srfi srfi-128)
) ;import


(check-set-mode! 'report-failed)


;; set-replace!
;; 修改 set，将其中指定的元素替换（可变操作）。
;;
;; 语法
;; ----
;; (set-replace! set element)
;;
;; 参数
;; ----
;; set : set
;; 目标 set。
;;
;; element : any
;; 用来替换的元素。
;;
;; 返回值
;; ------
;; set
;; 返回修改后的 set（与传入的 set 是同一个对象）。
;;
;; 注意
;; ----
;; 此函数会修改原 set。
;;
;; 示例
;; ----
;; (set-replace! (set 1) 1) => 修改后的 set，包含 1


(define s-1 (set 1))


;; Test basic replace!
(define s-mut-replace (set-copy s-1))
(set-replace! s-mut-replace 1)
(check (set-size s-mut-replace) => 1)
(check-true (set-contains? s-mut-replace 1)
) ;check-true


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
(define s-str-ci-mut
  (list->set-with-comparator string-ci-comparator
    '("Apple" "Banana")
  ) ;list->set-with-comparator
) ;define
(check (set-member s-str-ci-mut
         "apple"
         'not-found
       ) ;set-member
  =>
  "Apple"
) ;check


(set-replace! s-str-ci-mut "apple")
(check (set-member s-str-ci-mut
         "apple"
         'not-found
       ) ;set-member
  =>
  "apple"
) ;check


;; Test non-existing element
(set-replace! s-str-ci-mut "Pear")
(check (set-size s-str-ci-mut) => 2)


;; Test type error
(check-catch 'type-error
  (set-replace! "not a set" 1)
) ;check-catch


(check-report)
