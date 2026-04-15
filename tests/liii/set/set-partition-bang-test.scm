(import (liii check)
  (liii error)
  (liii set)
) ;import


(check-set-mode! 'report-failed)


;; set-partition!
;; 可变划分，返回满足 predicate 的 set（原 set）与不满足 predicate 的新 set。
;;
;; 语法
;; ----
;; (set-partition! predicate set)
;;
;; 参数
;; ----
;; predicate : procedure
;; 划分条件。
;;
;; set : set
;; 目标 set。
;;
;; 返回值
;; ------
;; values
;; 返回两个值：修改后的 set 与不满足 predicate 的新 set。
;;
;; 注意
;; ----
;; 原 set 会被修改为仅包含满足 predicate 的元素。
;;
;; 示例
;; ----
;; (set-partition! even? (set 1 2 3 4)) => 修改后的 set（包含 2,4）和 包含 1,3 的新 set


(define s-empty (set))


;; Test basic behavior
(define s-partition-mut (set 1 2 3 4))
(call-with-values (lambda ()
                    (set-partition! even? s-partition-mut)
                  ) ;lambda
  (lambda (yes no)
    (check-true (eq? yes s-partition-mut))
    (check (set-size yes) => 2)
    (check (set-size no) => 2)
    (check-true (set-contains? yes 2))
    (check-true (set-contains? yes 4))
    (check-true (set-contains? no 1))
    (check-true (set-contains? no 3))
  ) ;lambda
) ;call-with-values


;; Test empty set
(define s-partition-empty
  (set-copy s-empty)
) ;define
(call-with-values (lambda ()
                    (set-partition! even? s-partition-empty)
                  ) ;lambda
  (lambda (yes no)
    (check (set-size yes) => 0)
    (check (set-size no) => 0)
  ) ;lambda
) ;call-with-values


(check-catch 'type-error
  (set-partition! even? "not a set")
) ;check-catch


(check-report)
