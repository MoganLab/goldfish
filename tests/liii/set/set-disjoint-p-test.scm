(import (liii check)
  (liii error)
  (liii set)
  (srfi srfi-128)
) ;import


(check-set-mode! 'report-failed)


;; set-disjoint?
;; 检查两个 set 是否不相交（没有共同元素）。
;;
;; 语法
;; ----
;; (set-disjoint? set1 set2)
;;
;; 参数
;; ----
;; set1, set2 : set
;; 要检查的 set。
;;
;; 返回值
;; ----
;; boolean
;; 如果两个 set 没有共同元素，返回 #t；否则返回 #f。
;;
;; 注意
;; ----
;; 两个 set 的比较器必须相同，否则会抛出 value-error。
;;
;; 示例
;; ----
;; (set-disjoint? (set 1 2 3) (set 4 5)) => #t
;; (set-disjoint? (set 1 2 3) (set 2 3 4)) => #f
;;
;; 错误处理
;; ----
;; type-error
;; 当任一参数不是 set 时抛出。
;; value-error
;; 当两个 set 的比较器不同时抛出。


(define s-empty (set))
(define s-1 (set 1))
(define s-1-2-3 (set 1 2 3))
(define s-2-3-4 (set 2 3 4))
(define s-4-5 (set 4 5))


(check-true (set-disjoint? s-1-2-3 s-4-5)
) ;check-true
(check-false (set-disjoint? s-1-2-3 s-2-3-4)
) ;check-false
(check-true (set-disjoint? s-empty s-1))
(check-true (set-disjoint? s-1 s-empty))
(check-true (set-disjoint? s-empty s-empty)
) ;check-true
(check-catch 'type-error
  (set-disjoint? "not a set" s-1)
) ;check-catch
(check-catch 'type-error
  (set-disjoint? s-1 "not a set")
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
  (set-disjoint? s-1 s-str)
) ;check-catch


(check-report)
