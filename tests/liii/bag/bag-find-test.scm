(import (liii check)
        (liii bag)
        (liii error))

(check-set-mode! 'report-failed)

;; Data Setup
(define b-1-2 (bag 1 2 2))

#|
bag-find
查找满足条件的元素。

语法
----
(bag-find predicate bag failure)

参数
----
predicate : procedure
判断函数，接收元素并返回布尔值。

bag : bag
目标 bag。

failure : procedure
未找到时调用的过程。

返回值
-----
返回第一个满足 predicate 的元素，否则返回 failure 的结果。
|#
(check (bag-find even? b-1-2 (lambda () 'none)) => 2)
(check (bag-find (lambda (x) (> x 9)) b-1-2 (lambda () 'missing)) => 'missing)
(check-catch 'type-error (bag-find even? "not a bag" (lambda () 'none)))

(check-report)
