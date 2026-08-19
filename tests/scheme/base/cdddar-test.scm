(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; cdddar
;; cdddar 是 Scheme 内置函数，等价于 (cdr (cdr (cdr (car pair))))。
;;
;; 语法
;; ----
;; (cdddar pair)
;;
;; 参数
;; ----
;; pair : pair?
;; 嵌套的序对或列表，如 ((a b c d e))。
;;
;; 返回值
;; ------
;; 任意类型
;; 返回 (cdr (cdr (cdr (car pair))))。
;;
;; 错误处理
;; --------
;; wrong-type-arg
;; 参数不是序对，或者 car/cdr 链上的某一层不是序对时抛出错误。
(check (cdddar '((1 2 3 4 5))) => '(4 5))
(check (cdddar '((a b c d) e)) => '(d))
(check (cdddar (cons (cons 1 (cons 2 (cons 3 (cons 4 5)))) 6)) => '(4 . 5))
(check (cdddar '((1 2 3))) => '())
(check-catch 'wrong-type-arg (cdddar '(1 2 3)))
(check-catch 'wrong-type-arg (cdddar 'a))
(check-catch 'wrong-type-arg (cdddar '()))
(check-report)
