(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; caaddr
;; caaddr 是 Scheme 内置函数，等价于 (car (car (cdr (cdr pair))))。
;;
;; 语法
;; ----
;; (caaddr pair)
;;
;; 参数
;; ----
;; pair : pair?
;; 嵌套的序对或列表，如 (1 2 (a b))。
;;
;; 返回值
;; ------
;; 任意类型
;; 返回 (car (car (cdr (cdr pair))))。
;;
;; 错误处理
;; --------
;; wrong-type-arg
;; 参数不是序对，或者 cdr/car 链上的某一层不是序对时抛出错误。
(check (caaddr '(1 2 (3 4))) => 3)
(check (caaddr '(a b (c d) e)) => 'c)
(check (caaddr (cons 1 (cons 2 (cons (cons 'x 'y) 3)))) => 'x)
(check-catch 'wrong-type-arg (caaddr '(1 2 3)))
(check-catch 'wrong-type-arg (caaddr '(1 2 ())))
(check-catch 'wrong-type-arg (caaddr '(1 2)))
(check-catch 'wrong-type-arg (caaddr 'a))
(check-catch 'wrong-type-arg (caaddr '()))
(check-report)
