(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; list? 基本测试：空列表和各种简单列表
(check-true (list? '()))
(check-true (list? '(a)))
(check-true (list? '(a b c)))
(check-true (list? '(1 2 3 4 5)))
;; list? 嵌套和复杂结构测试
(check-true (list? '(a (b) c)))
(check-true (list? '((a) (b) (c))))
(check-true (list? '((a b) (c d))))
(check-true (list? '(1 (2 (3 (4))))))
;; list? 混合类型元素测试
(check-true (list? '(a 1 "string" #t)))
(check-true (list? '((list 1 2) (vector 3 4))))
;; list? 点和边界情况
(check-true (list? '(1 . 2)))
(check-true (list? '(a b . c)))
;; list? 特殊结构测试
(check-true (list? (let ((x '(1 2 3))) (set-cdr! (cddr x) x) x))
) ;check-true
;; list? 非列表类型测试 - 全面覆盖
(check-false (list? #t))
(check-false (list? #f))
(check-false (list? 123))
(check-false (list? -456))
(check-false (list? 0))
(check-false (list? 3.14))
(check-false (list? "Hello"))
(check-false (list? ""))
(check-false (list? '#()))
(check-false (list? '#(1 2 3)))
(check-false (list? '12345))
(check-false (list? 'symbol))
(check-false (list? #\a))
;; list? 错误处理测试
(check-catch 'wrong-number-of-args (list?))
(check-catch 'wrong-number-of-args (list? #t #f))
(check-report)