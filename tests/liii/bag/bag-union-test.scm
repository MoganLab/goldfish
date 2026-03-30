(import (liii check)
        (liii bag)
        (liii error))

(check-set-mode! 'report-failed)

#|
bag-union / bag-union!
多重集并集。

语法
----
(bag-union bag1 bag2 ...)
(bag-union! bag1 bag2 ...)

参数
----
bag1, bag2 ... : bag
参与运算的 bag。

返回值
-----
返回新的 bag，不修改原 bag（非破坏性版本）。
返回修改后的 bag1（破坏性版本）。
|#
(let* ((b1 (bag 'a 'a 'b))
       (b2 (bag 'a 'b 'b 'c))
       (u (bag-union b1 b2)))
  (check (bag-count (lambda (x) (eq? x 'a)) u) => 2)
  (check (bag-count (lambda (x) (eq? x 'b)) u) => 2)
  (check (bag-count (lambda (x) (eq? x 'c)) u) => 1))

(let* ((b1 (bag 'a 'a 'b))
       (b2 (bag 'a 'b 'b 'c)))
  (bag-union! b1 b2)
  (check (bag-count (lambda (x) (eq? x 'a)) b1) => 2)
  (check (bag-count (lambda (x) (eq? x 'b)) b1) => 2)
  (check (bag-count (lambda (x) (eq? x 'c)) b1) => 1))

(check-report)
