(import (liii check)
        (liii bag)
        (liii error))

(check-set-mode! 'report-failed)

#|
bag=? / bag<? / bag>? / bag<=? / bag>=?
多重集包含关系与相等性判断。

语法
----
(bag=? bag1 bag2 ...)
(bag<? bag1 bag2 ...)
(bag>? bag1 bag2 ...)
(bag<=? bag1 bag2 ...)
(bag>=? bag1 bag2 ...)

参数
----
bag1, bag2 ... : bag
参与比较的 bag。

返回值
-----
返回 #t 或 #f。
|#
(let ((b1 (bag 1 1 2))
      (b2 (bag 1 1 2 2))
      (b3 (bag 1 1 2)))
  (check-true (bag=? b1 b3))
  (check-false (bag=? b1 b2))
  (check-true (bag<=? b1 b2))
  (check-false (bag<=? b2 b1))
  (check-true (bag<? b1 b2))
  (check-false (bag<? b1 b1))
  (check-true (bag>=? b2 b1))
  (check-true (bag>? b2 b1)))

(check-report)
