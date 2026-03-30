(import (liii check)
        (liii error)
        (liii set)
) ;import

(check-set-mode! 'report-failed)

;; set-partition
;; 将 set 划分为满足 predicate 与不满足 predicate 的两个新 set。
;;
;; 语法
;; ----
;; (set-partition predicate set)
;;
;; 参数
;; ----
;; predicate : procedure
;; 划分条件。
;;
;; set : set
;; 源 set。
;;
;; 返回值
;; ------
;; values
;; 返回两个值：满足 predicate 的新 set 与不满足 predicate 的新 set。
;;
;; 注意
;; ----
;; 原 set 不会被修改。
;;
;; 示例
;; ----
;; (set-partition even? (set 1 2 3 4)) => 包含 2,4 的 set 和 包含 1,3 的 set

(define s-empty (set))

;; Test basic behavior
(define s-partition-1 (set 1 2 3 4))
(call-with-values
  (lambda () (set-partition even? s-partition-1))
  (lambda (yes no)
    (check-true (set? yes))
    (check-true (set? no))
    (check-true (eq? (set-element-comparator yes) (set-element-comparator s-partition-1)))
    (check-true (eq? (set-element-comparator no) (set-element-comparator s-partition-1)))
    (check (set-size yes) => 2)
    (check (set-size no) => 2)
    (check-true (set-contains? yes 2))
    (check-true (set-contains? yes 4))
    (check-true (set-contains? no 1))
    (check-true (set-contains? no 3))
    (check-true (set-contains? s-partition-1 2)) ; Original set unchanged
    (check-true (set-contains? s-partition-1 4))
  ) ;lambda
) ;call-with-values

;; Test empty set
(call-with-values
  (lambda () (set-partition even? s-empty))
  (lambda (yes no)
    (check (set-size yes) => 0)
    (check (set-size no) => 0)
  ) ;lambda
) ;call-with-values

(check-catch 'type-error (set-partition even? "not a set"))

(check-report)
