(import (liii check)
        (liii bag)
        (liii error))

(check-set-mode! 'report-failed)

;; Data Setup
(define b-empty (bag))

#|
bag-disjoint?
判断两个 bag 是否不相交。

语法
----
(bag-disjoint? bag1 bag2)

参数
----
bag1 : bag
第一个 bag。

bag2 : bag
第二个 bag。

返回值
-----
如果两个 bag 没有相等元素，返回 #t；否则返回 #f。
|#
(check-true (bag-disjoint? (bag 1 1) (bag 2 2)))
(check-false (bag-disjoint? (bag 1 1) (bag 1 2)))
(check-true (bag-disjoint? b-empty (bag 1)))
(check-true (bag-disjoint? (bag 1) b-empty))
(check-catch 'type-error (bag-disjoint? "not a bag" (bag 1)))
(check-catch 'type-error (bag-disjoint? (bag 1) "not a bag"))

(check-report)
