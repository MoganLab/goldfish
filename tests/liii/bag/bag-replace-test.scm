(import (liii check)
        (liii bag)
        (liii error))

(check-set-mode! 'report-failed)

#|
bag-replace / bag-replace!
替换 bag 中与元素等价的代表值，保留计数。

语法
----
(bag-replace bag element)
(bag-replace! bag element)

参数
----
bag : bag
目标 bag。

element : any
用于替换的元素（按 comparator 等价判断）。

返回值
-----
bag-replace 返回新的 bag，原 bag 保持不变（非破坏性）。
bag-replace! 就地修改原 bag，并返回修改后的 bag（破坏性）。
|#
(let* ((s1 "hello")
       (s2 (string-copy s1))
       (b (bag s1))
       (b2 (bag-replace b s2)))
  (check-true (eq? (car (bag->list b)) s1))
  (check-true (eq? (car (bag->list b2)) s2)))
(let* ((s1 "hello")
       (s2 (string-copy s1))
       (b (bag s1)))
  (bag-replace! b s2)
  (check-true (eq? (car (bag->list b)) s2)))

(check-report)
