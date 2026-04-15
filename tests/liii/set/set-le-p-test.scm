(import (liii check)
  (liii error)
  (liii set)
  (srfi srfi-128)
) ;import


(check-set-mode! 'report-failed)


;; set<=?
;; 检查一个 set 是否为另一个 set 的子集。
;;
;; 语法
;; ----
;; (set<=? set1 set2 ...)
;;
;; 参数
;; ----
;; set1, set2, ... : set
;; 要检查的 set。
;;
;; 返回值
;; ----
;; boolean
;; 如果每个 set 都是其后一个 set 的子集，返回 #t；否则返回 #f。
;;
;; 注意
;; ----
;; 子集意味着 set1 的所有元素都在 set2 中。
;; 比较器必须相同。
;;
;; 示例
;; ----
;; (set<=? (set) (set 1)) => #t
;; (set<=? (set 1) (set 1 2)) => #t
;;
;; 错误处理
;; ----
;; type-error
;; 当任一参数不是 set 时抛出。
;; value-error
;; 当 set 的比较器不同时抛出。


(define s-empty (set))
(define s-1 (set 1))
(define s-1-2 (set 1 2))
(define s-1-2-3 (set 1 2 3))


(check-true (set<=? s-empty s-1))
(check-true (set<=? s-1 s-1-2))
(check-true (set<=? s-1-2 s-1-2-3))
(check-true (set<=? s-1-2 s-1-2))
(check-false (set<=? s-1-2 s-1))
;; Chain
(check-true (set<=? s-empty s-1 s-1-2 s-1-2-3)
) ;check-true
(check-false (set<=? s-empty s-1-2 s-1))
(check-catch 'type-error
  (set<=? "not a set" s-1)
) ;check-catch


;; Test comparator mismatch
(define str-comp
  (make-comparator string?
    string=?
    string<?
    string-hash
  ) ;make-comparator
) ;define
(define s-str
  (list->set-with-comparator str-comp
    '("apple" "banana")
  ) ;list->set-with-comparator
) ;define
(check-catch 'value-error
  (set<=? s-1 s-str)
) ;check-catch


(check-report)
