(import (liii check)
        (liii bag)
        (liii error))

(check-set-mode! 'report-failed)

#|
bag-delete / bag-delete!
删除一个元素实例。

语法
----
(bag-delete bag element ...)
(bag-delete! bag element ...)

参数
----
bag : bag
目标 bag。

element ... : any
要删除的元素，每个元素只删除一个实例。

返回值
-----
bag-delete 返回新的 bag，原 bag 保持不变（非破坏性）。
bag-delete! 就地修改原 bag，并返回修改后的 bag（破坏性）。
|#
(let ((b (bag 1 2 2 3)))
  (define b2 (bag-delete b 2 3))
  (check (bag-size b) => 4)
  (check (bag-size b2) => 2)
  (check (bag-count (lambda (x) (= x 2)) b2) => 1))
(let ((b (bag 1 2 2 3)))
  (bag-delete! b 2 3)
  (check (bag-size b) => 2)
  (check (bag-count (lambda (x) (= x 2)) b) => 1))

(check-report)
