(import (liii check)
  (liii error)
  (liii set)
) ;import


(check-set-mode! 'report-failed)


;; set-remove!
;; 可变筛选，返回仅包含不满足 predicate 的元素的 set。
;;
;; 语法
;; ----
;; (set-remove! predicate set)
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
;; (set-remove! even? (set 1 2 3 4)) => 修改后的 set，仅包含 1, 3


(define s-empty (set))


;; Test basic behavior
(define s-remove-mut (set 1 2 3 4))
(define s-remove-mut-result
  (set-remove! even? s-remove-mut)
) ;define
(check-true (eq? s-remove-mut-result s-remove-mut)
) ;check-true
(check (set-size s-remove-mut) => 2)
(check-true (set-contains? s-remove-mut 1)
) ;check-true
(check-true (set-contains? s-remove-mut 3)
) ;check-true
(check-false (set-contains? s-remove-mut 2)
) ;check-false
(check-false (set-contains? s-remove-mut 4)
) ;check-false


;; Test empty set
(define s-remove-mut-empty
  (set-copy s-empty)
) ;define
(set-remove! even? s-remove-mut-empty)
(check (set-size s-remove-mut-empty)
  =>
  0
) ;check


(check-catch 'type-error
  (set-remove! even? "not a set")
) ;check-catch


(check-report)
