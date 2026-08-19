(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; cadadr
;; cadadr 是 Scheme 内置函数，等价于 (car (cdr (car (cdr pair))))。
;;
;; 语法
;; ----
;; (cadadr pair)
;;
;; 参数
;; ----
;; pair : pair?
;; 嵌套的序对或列表，如 (1 (a b c))。
;;
;; 返回值
;; ------
;; 任意类型
;; 返回 (car (cdr (car (cdr pair))))。
;;
;; 错误处理
;; --------
;; wrong-type-arg
;; 参数不是序对，或者 cdr/car 链上的某一层不是序对时抛出错误。
(check (cadadr '(1 (2 3 4))) => 3)
(check (cadadr '(a (b c) d)) => 'c)
(check (cadadr (cons 0 (cons (cons 1 (cons 2 3)) 4))) => 2)
(check-catch 'wrong-type-arg (cadadr '(1 (2))))
(check-catch 'wrong-type-arg (cadadr '(1 2)))
(check-catch 'wrong-type-arg (cadadr 'a))
(check-catch 'wrong-type-arg (cadadr '()))
(check-report)
