(import (liii check)
        (liii error)
        (liii set)
) ;import

(check-set-mode! 'report-failed)

;; set-filter
;; 返回一个新的 set，仅包含满足 predicate 的元素。
;;
;; 语法
;; ----
;; (set-filter predicate set)
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
;; (set-filter even? (set 1 2 3 4)) => 包含 2, 4 的 set

(define s-empty (set))

;; Test basic filtering
(define s-filter-1 (set 1 2 3 4))
(define s-filter-2 (set-filter even? s-filter-1))
(check-true (set? s-filter-2))
(check-true (eq? (set-element-comparator s-filter-2) (set-element-comparator s-filter-1)))
(check (set-size s-filter-2) => 2)
(check-true (set-contains? s-filter-2 2))
(check-true (set-contains? s-filter-2 4))
(check-true (set-contains? s-filter-1 1)) ; Original set unchanged
(check-true (set-contains? s-filter-1 3))

;; Test empty set
(define s-filter-empty (set-filter even? s-empty))
(check (set-size s-filter-empty) => 0)

(check-catch 'type-error (set-filter even? "not a set"))

(check-report)
