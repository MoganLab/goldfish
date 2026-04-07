(import (liii check)
        (liii list)
        (liii vector)
) ;import

(check-set-mode! 'report-failed)

;; reverse-list->vector
;; 将列表转换为反向向量。
;;
;; 语法
;; ----
;; (reverse-list->vector lst)
;;
;; 参数
;; ----
;; lst : list?
;; 要转换的列表。
;;
;; 返回值
;; ----
;; vector
;; 一个新的向量，元素顺序与输入列表相反。
;;
;; 注意
;; ----
;; 输入必须是正规列表，循环列表不被接受。
;;
;; 示例
;; ----
;; (reverse-list->vector '(1 2 3)) => #(3 2 1)
;; (reverse-list->vector '()) => #()
;;
;; 错误处理
;; ----
;; type-error 当lst不是正规列表时

(check (reverse-list->vector '()) => '#())
(check (reverse-list->vector '(1 2 3)) => '#(3 2 1))
(check (reverse-list->vector '(a b c)) => '#(c b a))
(check (reverse-list->vector '(42)) => '#(42))
(check (reverse-list->vector '(1 2.5 "hello" symbol #\c #t #f)) => '#(#f #t #\c symbol "hello" 2.5 1))
(check (reverse-list->vector '((1 2) (3 4))) => '#((3 4) (1 2)))
(check-catch 'type-error (reverse-list->vector 'not-a-list))
(check-catch 'type-error (reverse-list->vector '(1 2 . 3)))
(check-catch 'type-error (reverse-list->vector (circular-list 1 2 3)))

(check-report)
