(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; caadr
;; caadr 是 Scheme 内置函数，等价于 (car (car (cdr pair)))。
;;
;; 语法
;; ----
;; (caadr pair)
;;
;; 参数
;; ----
;; pair : pair?
;; 二层以上嵌套的序对或列表，如 (1 (a b))。
;;
;; 返回值
;; ------
;; 任意类型
;; 返回 (car (car (cdr pair)))。
;;
;; 错误处理
;; --------
;; type-error
;; 参数不是序对，或者 cdr/car 链上的某一层不是序对时抛出错误。
(check (caadr '(1 (2 3))) => 2)
(check (caadr '(1 (a b) c)) => 'a)
(check (caadr (cons 0 (cons (cons 'x 'y) 2))) => 'x)
(check-catch 'type-error (caadr '(1 ())))
(check-catch 'type-error (caadr '(1 2)))
(check-catch 'type-error (caadr '((1 2))))
(check-catch 'type-error (caadr 'a))
(check-catch 'type-error (caadr "hello"))
(check-catch 'type-error (caadr '()))
(check-report)
