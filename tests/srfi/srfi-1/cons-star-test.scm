;; cons*
;; 以线性更新方式构造列表，支持可变参数。
;;
;; 语法
;; ----
;; (cons* obj ...)
;;
;; 参数
;; ----
;; obj : any
;; 任意数量的参数，最少需要一个。
;;
;; 返回值
;; ------
;; list | pair
;; - 如果只有一个参数obj，返回该obj本身
;; - 如果有两个参数obj1 obj2，返回 (obj1 . obj2)
;; - 如果有三个或以上参数，返回一个列表，其中最后一个参数作为cdr

(import (srfi srfi-1) (liii check))

(check-set-mode! 'report-failed)

(check (cons* 1 2) => '(1 . 2))
(check (cons* 1 2 3) => '(1 2 . 3))
(check (cons* 'a 'b 'c 'd) => '(a b c . d))
(check (cons* '(1 2 3)) => '(1 2 3))
(check (cons* '(1 2) 3 4) => '((1 2) 3 . 4))
(check (cons* 1 2 '(3 4)) => '(1 2 3 4))
(check (cons* '(1) '(2) '(3)) => '((1) (2) 3))
(check (cons* 1 '() 3) => '(1 () . 3))
(check (cons* 1 (cons* 2 3)) => '(1 2 . 3))

(check (cons* 1) => 1)
(check (cons* 'a) => 'a)
(check (cons* '()) => '())
(check (cons* '(1 2 3)) => '(1 2 3))
(check (cons* '(a b) '(c d)) => '((a b) c d))

(check (cons* 1 2 '()) => '(1 2))
(check (cons* '() '() '()) => '(() ()))
(check (cons* 1 2 3 4 '()) => '(1 2 3 4))
(check (cons* 1 2 3 4 5) => '(1 2 3 4 . 5))

(check (cons* 1 2 (cons* 3 4 5)) => '(1 2 3 4 . 5))
(check (cons* (cons* 1 2) 3 4) => '((1 . 2) 3 . 4))
(check (cons* 1 (list 2 3) (cons* 4 5)) => '(1 (2 3) 4 . 5))

(check (cons* 'a 'b 'c) => '(a b . c))
(check (cons* 1 2 3 4 5 6) => '(1 2 3 4 5 . 6))

(check (cons* 1 'a 2 'b) => '(1 a 2 . b))
(check (cons* 'hello 42 'world) => '(hello 42 . world))

(check-catch 'wrong-number-of-args (cons*))

(check-report)
