(import (liii check)
        (liii vector))

(check-set-mode! 'report-failed)

;; list->vector
;; 将列表转换为向量。
;;
;; 语法
;; ----
;; (list->vector lst)
;;
;; 参数
;; ----
;; lst : list?
;; 要转换的列表。
;;
;; 返回值
;; ----
;; vector
;; 一个新的向量，元素顺序与列表一致。
;;
;; 注意
;; ----
;; 输入必须是正规列表。
;;
;; 示例
;; ----
;; (list->vector '(a b c)) => #(a b c)
;; (list->vector '()) => #()
;;
;; 错误处理
;; ----
;; wrong-type-arg 当lst不是正规列表时

(check (list->vector '()) => #())
(check (list->vector '(a b c)) => #(a b c))
(check (list->vector '(1 2 3)) => #(1 2 3))
(check (list->vector '(42)) => #(42))
(check (list->vector '(a)) => #(a))
(check (list->vector '(1 2.5 "hello" symbol #\c #t #f)) => #(1 2.5 "hello" symbol #\c #t #f))
(check (list->vector '((1 2) (3 4))) => #((1 2) (3 4)))
(check-catch 'wrong-type-arg (list->vector 'not-a-list))
(check-catch 'wrong-type-arg (list->vector '(1 2 . 3)))

(check-report)
