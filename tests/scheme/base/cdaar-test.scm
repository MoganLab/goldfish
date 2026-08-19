(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; cdaar
;; cdaar 是 Scheme 内置函数，等价于 (cdr (car (car pair)))。
;;
;; 语法
;; ----
;; (cdaar pair)
;;
;; 参数
;; ----
;; pair : pair?
;; 三层嵌套的序对或列表，如 (((a b c)) d)。
;;
;; 返回值
;; ------
;; 任意类型
;; 返回 (cdr (car (car pair)))。
;;
;; 错误处理
;; --------
;; wrong-type-arg
;; 参数不是序对，或者 car 链上的某一层不是序对时抛出错误。
(check (cdaar '(((1 2 3)))) => '(2 3))
(check (cdaar '(((a b c) d) e)) => '(b c))
(check (cdaar (cons (cons (cons 1 2) 3) 4)) => 2)
(check-catch 'wrong-type-arg (cdaar '((1 2))))
(check-catch 'wrong-type-arg (cdaar '(1 2 3)))
(check-catch 'wrong-type-arg (cdaar 'a))
(check-catch 'wrong-type-arg (cdaar '()))
(check-report)
