(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; cddddr
;; cddddr 是 Scheme 内置函数，等价于 (cdr (cdr (cdr (cdr pair))))。
;;
;; 语法
;; ----
;; (cddddr pair)
;;
;; 参数
;; ----
;; pair : pair?
;; 至少含有四个元素的序对或列表。
;;
;; 返回值
;; ------
;; 任意类型
;; 返回 (cdr (cdr (cdr (cdr pair))))，即列表第四个元素之后的部分。
;;
;; 错误处理
;; --------
;; wrong-type-arg
;; 参数不是序对，或者 cdr 链上的某一层不是序对时抛出错误。
(check (cddddr '(1 2 3 4 5)) => '(5))
(check (cddddr '(a b c d e f)) => '(e f))
(check (cddddr (cons 1 (cons 2 (cons 3 (cons 4 5))))) => 5)
(check (cddddr '(1 2 3 4)) => '())
(check-catch 'wrong-type-arg (cddddr '(1 2 3)))
(check-catch 'wrong-type-arg (cddddr '(1 2)))
(check-catch 'wrong-type-arg (cddddr 'a))
(check-catch 'wrong-type-arg (cddddr '()))
(check-report)
