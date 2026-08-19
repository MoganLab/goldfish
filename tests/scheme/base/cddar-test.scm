(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; cddar
;; cddar 是 Scheme 内置函数，等价于 (cdr (cdr (car pair)))。
;;
;; 语法
;; ----
;; (cddar pair)
;;
;; 参数
;; ----
;; pair : pair?
;; 二层以上嵌套的序对或列表，如 ((a b c d))。
;;
;; 返回值
;; ------
;; 任意类型
;; 返回 (cdr (cdr (car pair)))。
;;
;; 错误处理
;; --------
;; wrong-type-arg
;; 参数不是序对，或者 car/cdr 链上的某一层不是序对时抛出错误。
(check (cddar '((1 2 3 4))) => '(3 4))
(check (cddar '((a b c) d)) => '(c))
(check (cddar (cons (cons 1 (cons 2 3)) 4)) => 3)
(check (cddar '((1 2))) => '())
(check-catch 'wrong-type-arg (cddar '(1 2 3)))
(check-catch 'wrong-type-arg (cddar 'a))
(check-catch 'wrong-type-arg (cddar '()))
(check-report)
