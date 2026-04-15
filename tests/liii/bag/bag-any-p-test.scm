(import (liii check)
  (liii bag)
  (liii error)
) ;import

(check-set-mode! 'report-failed)

;; Data Setup
(define b-empty (bag))
(define b-1-2 (bag 1 2 2))

;; bag-any? 函数测试
;;
;; 语法
;; ----
;; (bag-any? predicate bag)
;;
;; 参数
;; ----
;; predicate : procedure
;; 判断函数，接收元素并返回布尔值。
;;
;; bag : bag
;; 目标 bag。
;;
;; 返回值
;; -----
;; 如果存在满足 predicate 的元素，返回 #t，否则返回 #f。

(check-true (bag-any? even? b-1-2))
(check-false (bag-any? (lambda (x) (> x 9)) b-1-2)
) ;check-false
(check-false (bag-any? even? b-empty))
(check-catch 'type-error
  (bag-any? even? "not a bag")
) ;check-catch

(check-report)
