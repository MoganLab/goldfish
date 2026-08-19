(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; cadar
;; cadar 是 Scheme 内置函数，等价于 (car (cdr (car pair)))。
;;
;; 语法
;; ----
;; (cadar pair)
;;
;; 参数
;; ----
;; pair : pair?
;; 二层以上嵌套的序对或列表，如 ((a b c) d)。
;;
;; 返回值
;; ------
;; 任意类型
;; 返回 (car (cdr (car pair)))。
;;
;; 错误处理
;; --------
;; wrong-type-arg
;; 参数不是序对，或者 car/cdr 链上的某一层不是序对时抛出错误。
(check (cadar '((1 2 3))) => 2)
(check (cadar '((a b c) d)) => 'b)
(check (cadar (cons (cons 1 (cons 2 3)) 4)) => 2)
(check-catch 'wrong-type-arg (cadar '((1))))
(check-catch 'wrong-type-arg (cadar '(1 2 3)))
(check-catch 'wrong-type-arg (cadar 'a))
(check-catch 'wrong-type-arg (cadar 123))
(check-catch 'wrong-type-arg (cadar '()))
(check-report)
