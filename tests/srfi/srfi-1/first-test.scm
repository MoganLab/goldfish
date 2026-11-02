;; first
;; 获取列表的第一个元素。
;;
;; 语法
;; ----
;; (first list)
;;
;; 参数
;; ----
;; list : pair?
;; 非空列表或点对结构。
;;
;; 返回值
;; ------
;; any
;; 返回列表的第一个元素(car部分)。

(import (srfi srfi-1) (liii check))

(check-set-mode! 'report-failed)

(check (first '(1 2 3 4 5 6 7 8 9 10)) => 1)
(check (first '(left . right)) => 'left)
(check (first '(a b c)) => 'a)
(check (first '(42)) => 42)

(check-catch 'type-error (first '()))

(check (first '(a)) => 'a)
(check (first '(a b)) => 'a)
(check (first '(a b c d e)) => 'a)

(check (first '(a . b)) => 'a)
(check (first '((1 2) 3 4)) => '(1 2))

(check (first '((a b) (c d))) => '(a b))
(check (first '(() a b)) => '())

(check (first '((1 2 3) 4 5)) => '(1 2 3))
(check (first '("a" "b" "c")) => "a")
(check (first '(42 43 44)) => 42)
(check (first '(#t #f #t)) => #t)

(check (first '(1 "hello" a)) => 1)
(check (first '(a 1 #t)) => 'a)

(check-report)
