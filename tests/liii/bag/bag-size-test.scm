(import (liii check)
        (liii bag)
        (liii error))

(check-set-mode! 'report-failed)

;; Data Setup
(define b-empty (bag))
(define b-1-2 (bag 1 2 2))

#|
bag-size
返回 bag 中元素总数（含重复）。

语法
----
(bag-size bag)

参数
----
bag : bag
目标 bag。

返回值
-----
返回 bag 中元素总数（包含重复元素）。
|#
(check (bag-size b-empty) => 0)
(check (bag-size b-1-2) => 3)
(check-catch 'type-error (bag-size "not a bag"))

(check-report)
