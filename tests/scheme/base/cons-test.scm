(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; cons
;; 构造一个点对（pair）。
;;
;; 语法
;; ----
;; (cons obj1 obj2)
;;
;; 参数
;; ----
;; obj1 : any
;; 点对的首元素。
;; obj2 : any
;; 点对的尾元素。
;;
;; 返回值
;; ------
;; pair?
;; 一个以 obj1 为 car、obj2 为 cdr 的新点对。
;;
;; 说明
;; ----
;; 1. 如果 obj2 是列表，则返回一个扩展后的列表
;; 2. 如果 obj2 不是列表，则返回一个非列表的点对
(check (cons 'a '()) => '(a))
(check (cons '(a) '(b c d)) => '((a) b c d))
(check (cons "a" '(b c)) => '("a" b c))
(check (cons 'a 3) => '(a . 3))
(check (cons '(a b) 'c) => '((a b) . c))
(check (car (cons 'a 'b)) => 'a)
(check (cdr (cons 'a 'b)) => 'b)
(check (cons 1 (cons 2 (cons 3 '()))) => '(1 2 3))
(check-catch 'wrong-number-of-args (cons))
(check-catch 'wrong-number-of-args (cons 1))
(check-catch 'wrong-number-of-args (cons 1 2 3))

(check-report)
