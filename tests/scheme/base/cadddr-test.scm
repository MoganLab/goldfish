(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; cadddr
;; cadddr 是 Scheme 内置函数，等价于 (car (cdr (cdr (cdr pair))))。
;;
;; 语法
;; ----
;; (cadddr pair)
;;
;; 参数
;; ----
;; pair : pair?
;; 至少含有四个元素的序对或列表。
;;
;; 返回值
;; ------
;; 任意类型
;; 返回 (car (cdr (cdr (cdr pair))))，即列表的第四个元素。
;;
;; 错误处理
;; --------
;; wrong-type-arg
;; 参数不是序对，或者 cdr 链上的某一层不是序对时抛出错误。
(check (cadddr '(1 2 3 4 5)) => 4)
(check (cadddr '(a b c d)) => 'd)
(check (cadddr (cons 1 (cons 2 (cons 3 (cons 'x 'y))))) => 'x)
(check-catch 'wrong-type-arg (cadddr '(1 2 3)))
(check-catch 'wrong-type-arg (cadddr '(1 2)))
(check-catch 'wrong-type-arg (cadddr 'a))
(check-catch 'wrong-type-arg (cadddr '()))
(check-report)
