(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; caddar
;; caddar 是 Scheme 内置函数，等价于 (car (cdr (cdr (car pair))))。
;;
;; 语法
;; ----
;; (caddar pair)
;;
;; 参数
;; ----
;; pair : pair?
;; 嵌套的序对或列表，如 ((a b c d))。
;;
;; 返回值
;; ------
;; 任意类型
;; 返回 (car (cdr (cdr (car pair))))。
;;
;; 错误处理
;; --------
;; wrong-type-arg
;; 参数不是序对，或者 car/cdr 链上的某一层不是序对时抛出错误。
(check (caddar '((1 2 3 4))) => 3)
(check (caddar '((a b c) d)) => 'c)
(check (caddar (cons (cons 1 (cons 2 (cons 3 4))) 5)) => 3)
(check-catch 'wrong-type-arg (caddar '((1 2))))
(check-catch 'wrong-type-arg (caddar '(1 2 3)))
(check-catch 'wrong-type-arg (caddar 'a))
(check-catch 'wrong-type-arg (caddar '()))
(check-report)
