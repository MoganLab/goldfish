(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; cddadr
;; cddadr 是 Scheme 内置函数，等价于 (cdr (cdr (car (cdr pair))))。
;;
;; 语法
;; ----
;; (cddadr pair)
;;
;; 参数
;; ----
;; pair : pair?
;; 嵌套的序对或列表，如 (1 (a b c d))。
;;
;; 返回值
;; ------
;; 任意类型
;; 返回 (cdr (cdr (car (cdr pair))))。
;;
;; 错误处理
;; --------
;; wrong-type-arg
;; 参数不是序对，或者 cdr/car 链上的某一层不是序对时抛出错误。
(check (cddadr '(1 (2 3 4 5))) => '(4 5))
(check (cddadr '(a (b c d) e)) => '(d))
(check (cddadr (cons 0 (cons (cons 1 (cons 2 (cons 3 4))) 5))) => '(3 . 4))
(check (cddadr '(1 (2 3))) => '())
(check-catch 'wrong-type-arg (cddadr '(1 2)))
(check-catch 'wrong-type-arg (cddadr 'a))
(check-catch 'wrong-type-arg (cddadr '()))
(check-report)
