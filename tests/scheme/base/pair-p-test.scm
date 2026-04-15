(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; 测试 pair? 对各种序对结构的判断
(check-true (pair? '(a . b)))
(check-true (pair? '(a b c)))
(check-true (pair? (cons 1 2)))
(check-true (pair? (cons 'a (cons 'b 'c)))
) ;check-true
(check-false (pair? 'a))
(check-false (pair? 123))
(check-false (pair? "string"))
(check-false (pair? #t))
(check-false (pair? #f))
;; pair? 边界条件测试补充
;; 基本边界值验证
(check-false (pair? '()))
(check-true (pair? '(single)))
(check-true (pair? (cons 1 '())))
(check-true (pair? '(())))
;; 嵌套深度边界测试
(check-true (pair? '((((a))))))
(check-true (pair? (cons 'a (cons 'b (cons 'c '())))
            ) ;pair?
) ;check-true
(check-true (pair? '(a b (c d (e)))))
;; 数据类型兼容性边界测试
(check-false (pair? 42))
(check-false (pair? 3.14))
(check-false (pair? 1.0+2.0i))
(check-false (pair? #t))
(check-false (pair? #f))
(check-false (pair? "hello"))
(check-false (pair? #\a))
(check-false (pair? 'symbol))
(check-false (pair? 'quote))
(check-false (pair? +))
(check-false (pair? length))
;; 复杂对象边界测试
(check-false (pair? #(1 2 3)))
(check-false (pair? #u(1 2 3)))
(check-false (pair? (lambda (x) x)))
(check-false (pair? #<eof>))
;; 极端边界测试
(check-true (pair? (cons '() '())))
(check-true (pair? (cons #t #f)))
(check-true (pair? (cons 42 "string")))
(check-true (pair? (cons (cons 1 2) (cons 3 4)))
) ;check-true
;; 构造器多样化测试
(check-true (pair? (list 1 2)))
(check-true (pair? (append '(1) '(2))))
(check-true (pair? (cons 'a (list 'b 'c)))
) ;check-true
;; 结构性边界验证
(check-false (pair? 'a))
(check-false (pair? 1000000))
(check-false (pair? "中文测试"))
(check-false (pair? #\newline))
;; Improper list 边界验证
(check-true (pair? '(a . b)))
(check-true (pair? '(a b . c)))
(check-true (pair? '(a b c . d)))
(check-report)