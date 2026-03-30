(import (liii check)
        (liii bag)
        (liii error))

(check-set-mode! 'report-failed)

#|
bag-delete-all / bag-delete-all!
按列表删除元素实例（列表含重复则多次删除）。

语法
----
(bag-delete-all bag element-list)
(bag-delete-all! bag element-list)

参数
----
bag : bag
目标 bag。

element-list : list
要删除的元素列表，列表中重复元素会多次删除。

返回值
-----
bag-delete-all 返回新的 bag，原 bag 保持不变（非破坏性）。
bag-delete-all! 就地修改原 bag，并返回修改后的 bag（破坏性）。
|#
(let ((b (bag 1 2 2 2 3)))
  (define b2 (bag-delete-all b '(2 2 3)))
  (check (bag-size b) => 5)
  (check (bag-size b2) => 2)
  (check (bag-count (lambda (x) (= x 2)) b2) => 1))
(let ((b (bag 1 2 2 2 3)))
  (bag-delete-all! b '(2 2 3))
  (check (bag-size b) => 2)
  (check (bag-count (lambda (x) (= x 2)) b) => 1))

(check-report)
