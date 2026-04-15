(import (liii check)
  (liii error)
  (liii set)
) ;import


(check-set-mode! 'report-failed)


;; set-remove
;; 返回一个新的 set，仅包含不满足 predicate 的元素。
;;
;; 语法
;; ----
;; (set-remove predicate set)
;;
;; 参数
;; ----
;; predicate : procedure
;; 筛选条件。
;;
;; set : set
;; 源 set。
;;
;; 返回值
;; ------
;; set
;; 返回新的 set，比较器与原 set 相同。
;;
;; 注意
;; ----
;; 原 set 不会被修改。
;;
;; 示例
;; ----
;; (set-remove even? (set 1 2 3 4)) => 包含 1, 3 的 set


(define s-empty (set))


;; Test basic removal
(define s-remove-1 (set 1 2 3 4))
(define s-remove-2
  (set-remove even? s-remove-1)
) ;define
(check-true (set? s-remove-2))
(check-true (eq? (set-element-comparator s-remove-2)
              (set-element-comparator s-remove-1)
            ) ;eq?
) ;check-true
(check (set-size s-remove-2) => 2)
(check-true (set-contains? s-remove-2 1)
) ;check-true
(check-true (set-contains? s-remove-2 3)
) ;check-true
(check-false (set-contains? s-remove-2 2)
) ;check-false
(check-false (set-contains? s-remove-2 4)
) ;check-false
(check-true (set-contains? s-remove-1 2)
) ;check-true
(check-true (set-contains? s-remove-1 4)
) ;check-true


;; Test empty set
(define s-remove-empty
  (set-remove even? s-empty)
) ;define
(check (set-size s-remove-empty) => 0)


(check-catch 'type-error
  (set-remove even? "not a set")
) ;check-catch


(check-report)
