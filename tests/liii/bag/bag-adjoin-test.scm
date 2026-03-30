(import (liii check)
        (liii bag)
        (liii error))

(check-set-mode! 'report-failed)

#|
bag-adjoin / bag-adjoin!
向 bag 添加元素（允许重复）。

语法
----
(bag-adjoin bag element ...)
(bag-adjoin! bag element ...)

参数
----
bag : bag
目标 bag。

element ... : any
要添加的元素（可重复）。

返回值
-----
bag-adjoin 返回新的 bag，原 bag 保持不变（非破坏性）。
bag-adjoin! 就地修改原 bag，并返回修改后的 bag（破坏性）。
|#
(let ((b (bag 1 2 2)))
  (define b2 (bag-adjoin b 2 3))
  (check (bag-size b) => 3)
  (check (bag-size b2) => 5)
  (check (bag-count (lambda (x) (= x 2)) b2) => 3)
  (bag-adjoin! b 2 3)
  (check (bag-size b) => 5))
(check-catch 'type-error (bag-adjoin "not a bag" 1))
(check-catch 'type-error (bag-adjoin! "not a bag" 1))

(check-report)
