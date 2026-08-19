(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; cdadr
;; cdadr 是 Scheme 内置函数，等价于 (cdr (car (cdr pair)))。
;;
;; 语法
;; ----
;; (cdadr pair)
;;
;; 参数
;; ----
;; pair : pair?
;; 二层以上嵌套的序对或列表，如 (1 (a b c))。
;;
;; 返回值
;; ------
;; 任意类型
;; 返回 (cdr (car (cdr pair)))。
;;
;; 错误处理
;; --------
;; wrong-type-arg
;; 参数不是序对，或者 cdr/car 链上的某一层不是序对时抛出错误。
(check (cdadr '(1 (2 3 4))) => '(3 4))
(check (cdadr '(a (b c) d)) => '(c))
(check (cdadr (cons 0 (cons (cons 1 2) 3))) => 2)
(check (cdadr '(1 (2))) => '())
(check-catch 'wrong-type-arg (cdadr '(1 2)))
(check-catch 'wrong-type-arg (cdadr 'a))
(check-catch 'wrong-type-arg (cdadr '()))
(check-report)
