(import (liii check)
        (liii error)
        (liii set)
) ;import

(check-set-mode! 'report-failed)

;; set-filter!
;; 可变筛选，返回仅包含满足 predicate 的元素的 set。
;;
;; 语法
;; ----
;; (set-filter! predicate set)
;;
;; 参数
;; ----
;; predicate : procedure
;; 筛选条件。
;;
;; set : set
;; 目标 set。
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
;; (set-filter! odd? (set 1 2 3 4)) => 修改后的 set，仅包含 1, 3

(define s-empty (set))

;; Test basic behavior
(define s-filter-mut (set 1 2 3 4))
(define s-filter-mut-result (set-filter! odd? s-filter-mut))
(check-true (eq? s-filter-mut-result s-filter-mut))
(check (set-size s-filter-mut) => 2)
(check-true (set-contains? s-filter-mut 1))
(check-true (set-contains? s-filter-mut 3))
(check-false (set-contains? s-filter-mut 2))
(check-false (set-contains? s-filter-mut 4))

;; Test empty set
(define s-filter-mut-empty (set-copy s-empty))
(set-filter! even? s-filter-mut-empty)
(check (set-size s-filter-mut-empty) => 0)

(check-catch 'type-error (set-filter! even? "not a set"))

(check-report)
