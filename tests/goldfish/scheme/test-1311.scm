(import (liii check) (scheme base))

(check-set-mode! 'report-failed)

;; cons 测试
(check (cons 1 2) => '(1 . 2))
(check (cons 1 '()) => '(1))
(check (cons 1 '(2 3)) => '(1 2 3))
(check (cons '(a) '(b c d)) => '((a) b c d))

;; car 和 cdr 测试
(check (car '(1 2 3)) => 1)
(check (car (cons 'a 'b)) => 'a)
(check (cdr '(1 2 3)) => '(2 3))
(check (cdr '(1)) => '())
(check (cdr (cons 'a 'b)) => 'b)

;; list 测试
(check (list) => '())
(check (list 1) => '(1))
(check (list 1 2 3) => '(1 2 3))
(check (list 'a '(b c)) => '(a (b c)))

;; length 测试
(check (length '()) => 0)
(check (length '(1)) => 1)
(check (length '(1 2 3 4)) => 4)

;; append 测试
(check (append) => '())
(check (append '(1 2) '(3 4)) => '(1 2 3 4))
(check (append '(1) '(2) '(3)) => '(1 2 3))
(check (append '() '(1 2) '()) => '(1 2))

;; reverse 测试
(check (reverse '()) => '())
(check (reverse '(1)) => '(1))
(check (reverse '(1 2 3)) => '(3 2 1))
(check (reverse '(a b c d)) => '(d c b a))

(check-report)
