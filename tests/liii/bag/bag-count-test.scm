(import (liii check)
        (liii bag)
        (liii error)
) ;import

(check-set-mode! 'report-failed)

;; Data Setup
(define b-1-2 (bag 1 2 2))

;; bag-count 函数测试
;;
;; 语法
;; ----
;; (bag-count predicate bag)
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
;; 返回满足 predicate 的元素总数（含重复）。

(check (bag-count even? b-1-2) => 2)
(check (bag-count (lambda (x) (> x 9)) b-1-2) => 0)
(check-catch 'type-error (bag-count even? "not a bag"))

(check-report)
