(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; caddr
;; caddr 是 Scheme 内置函数，等价于 (car (cdr (cdr pair)))。
;;
;; 语法
;; ----
;; (caddr pair)
;;
;; 参数
;; ----
;; pair : pair?
;; 至少含有两个元素的序对或列表。
;;
;; 返回值
;; ------
;; 任意类型
;; 返回 (car (cdr (cdr pair)))，即列表的第三个元素。
;;
;; 错误处理
;; --------
;; wrong-type-arg
;; 参数不是序对，或者 cdr 链上的某一层不是序对时抛出错误。
(check (caddr '(1 2 3 4)) => 3)
(check (caddr '(a b c)) => 'c)
(check (caddr (cons 1 (cons 2 (cons 'x 'y)))) => 'x)
(check-catch 'wrong-type-arg (caddr '(1 2)))
(check-catch 'wrong-type-arg (caddr '(1)))
(check-catch 'wrong-type-arg (caddr 'a))
(check-catch 'wrong-type-arg (caddr 123))
(check-catch 'wrong-type-arg (caddr '()))
(check-report)
