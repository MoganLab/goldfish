(import (liii check)
        (liii bag)
        (liii error))

(check-set-mode! 'report-failed)

;; Data Setup
(define b-empty (bag))
(define b-1-2 (bag 1 2 2))

#|
bag-every?
判断是否所有元素都满足条件。

语法
----
(bag-every? predicate bag)

参数
----
predicate : procedure
判断函数，接收元素并返回布尔值。

bag : bag
目标 bag。

返回值
-----
如果 bag 中所有元素都满足 predicate 返回 #t，否则返回 #f。
空 bag 返回 #t。
|#
(check-true (bag-every? (lambda (x) (> x 0)) b-1-2))
(check-false (bag-every? even? b-1-2))
(check-true (bag-every? even? b-empty))
(check-catch 'type-error (bag-every? even? "not a bag"))

(check-report)
