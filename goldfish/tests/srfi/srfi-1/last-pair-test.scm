;; last-pair
;; 返回列表的最后一个点对。
;;
;; 语法
;; ----
;; (last-pair lst)
;;
;; 参数
;; ----
;; lst : pair?
;; 非空列表或点对结构。
;;
;; 返回值
;; ------
;; pair
;; 返回列表的最后一个点对（cons cell）。对于proper list，返回最后一个元素的点对；对于dotted list，返回包含最后元素的点对。

(import (srfi srfi-1) (liii check))

(check-set-mode! 'report-failed)

(check (last-pair '(c)) => '(c))
(check (last-pair '(b . c)) => '(b . c))

(check (last-pair '(a b c)) => '(c))
(check (last-pair '(1 2 3 4 5)) => '(5))
(check (last-pair '(a)) => '(a))

(check (last-pair '(x y z)) => '(z))
(check (last-pair '(42)) => '(42))
(check (last-pair '(() a b)) => '(b))

(check (last-pair '(a b . c)) => '(b . c))
(check (last-pair '(a . b)) => '(a . b))
(check (last-pair '(a b c . d)) => '(c . d))

(check (last-pair '((a b) (c d))) => '((c d)))
(check (last-pair '((a) b (c d))) => '((c d)))
(check (last-pair '(a (b c) d)) => '(d))

(check (last-pair '(a (b (c)))) => '((b (c))))
(check (last-pair '("hello" "world" "test")) => '("test"))
(check (last-pair '(1 "a" b)) => '(b))

(check (last-pair '(#t #f #t)) => '(#t))
(check (last-pair '(42 43 44.5)) => '(44.5))
(check (last-pair '(() [] {})) => '({}))

(check-catch 'type-error (last-pair '()))
(check-catch 'type-error (last-pair 'not-a-list))

(check-report)
