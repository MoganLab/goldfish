(import (liii check)
        (liii bag)
        (liii error))

(check-set-mode! 'report-failed)

;; Data Setup
(define b-empty (bag))
(define b-1-2 (bag 1 2 2))

#|
bag?
判断是否为 bag。

语法
----
(bag? obj)

参数
----
obj : any
要检查的对象。

返回值
-----
如果 obj 是 bag，返回 #t；否则返回 #f。
|#
(check-true (bag? b-empty))
(check-true (bag? b-1-2))
(check-false (bag? "not a bag"))
(check-false (bag? '()))

(check-report)
