(import (liii check)
  (liii error)
  (liii set)
  (srfi srfi-128)
) ;import


(check-set-mode! 'report-failed)


;; set-xor
;; 返回两个 set 的对称差集。
;;
;; 语法
;; ----
;; (set-xor set1 set2)
;;
;; 参数
;; ----
;; set1, set2 : set
;; 参与对称差集的 set。
;;
;; 返回值
;; ------
;; set
;; 返回新的 set。
;;
;; 注意
;; ----
;; 对称差集包含只在其中一个 set 中出现的元素。
;; 所有 set 的比较器必须相同。
;;
;; 示例
;; ----
;; (set-xor (set 1 2 3) (set 2 3 4)) => 包含 1, 4 的 set


(define s-empty (set))
(define s-1-2-3 (set 1 2 3))
(define s-2-3-4 (set 2 3 4))


;; Test basic xor
(define s-xor-1
  (set-xor s-1-2-3 s-2-3-4)
) ;define
(check (set-size s-xor-1) => 2)
(check-true (set-contains? s-xor-1 1))
(check-true (set-contains? s-xor-1 4))


;; Test element source (using case-insensitive comparator)
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
(define s-union-ci-1
  (list->set-with-comparator string-ci-comparator
    '("Apple")
  ) ;list->set-with-comparator
) ;define
(define s-union-ci-2
  (list->set-with-comparator string-ci-comparator
    '("apple" "Banana")
  ) ;list->set-with-comparator
) ;define
(define s-xor-ci
  (set-xor s-union-ci-1 s-union-ci-2)
) ;define
(check (set-size s-xor-ci) => 1)
(check (set-member s-xor-ci
         "banana"
         'not-found
       ) ;set-member
  =>
  "Banana"
) ;check
(check-true (set-contains? s-xor-ci "banana")
) ;check-true
(check-false (set-contains? s-xor-ci "apple")
) ;check-false


;; Test type and comparator errors
(check-catch 'type-error
  (set-xor "not a set" s-1-2-3)
) ;check-catch
(define s-str-ci
  (list->set-with-comparator string-ci-comparator
    '("test")
  ) ;list->set-with-comparator
) ;define
(check-catch 'value-error
  (set-xor s-1-2-3 s-str-ci)
) ;check-catch


(check-report)
