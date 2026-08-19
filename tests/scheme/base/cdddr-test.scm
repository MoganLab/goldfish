(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; cdddr
;; cdddr 是 Scheme 内置函数，等价于 (cdr (cdr (cdr pair)))。
;;
;; 语法
;; ----
;; (cdddr pair)
;;
;; 参数
;; ----
;; pair : pair?
;; 至少含有三个元素的序对或列表。
;;
;; 返回值
;; ------
;; 任意类型
;; 返回 (cdr (cdr (cdr pair)))，即列表第三个元素之后的部分。
;;
;; 错误处理
;; --------
;; wrong-type-arg
;; 参数不是序对，或者 cdr 链上的某一层不是序对时抛出错误。
(check (cdddr '(1 2 3 4)) => '(4))
(check (cdddr '(a b c d e)) => '(d e))
(check (cdddr (cons 1 (cons 2 (cons 3 4)))) => 4)
(check (cdddr '(1 2 3)) => '())
(check-catch 'wrong-type-arg (cdddr '(1 2)))
(check-catch 'wrong-type-arg (cdddr '(1)))
(check-catch 'wrong-type-arg (cdddr 'a))
(check-catch 'wrong-type-arg (cdddr '()))
(check-report)
