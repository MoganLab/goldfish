(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
(check (car '(a b c . d)) => 'a)
(check (car '(a b c)) => 'a)
;; car 边界条件测试
(check (car '(a)) => 'a)
(check (car '(1)) => 1)
(check (car '(#t)) => #t)
(check (car '("hello")) => "hello")
(check (car '(42)) => 42)
(check (car '(() b c)) => '())
;; 各种数据类型作为car值测试
(check (car '(123 "text" symbol)) => 123)
(check (car '(#\a #\b #\c)) => #\a)
(check (car '((a b) c d)) => '(a b))
(check (car '((((a))))) => '(((a))))
(check (car '("nested" (list) "structure")) => "nested")
;; 点对结构boundary测试
(check (car '(a . b)) => 'a)
(check (car (cons 1 2)) => 1)
(check (car (cons 'a (cons 'b 'c))) => 'a)
(check-catch 'wrong-type-arg (car '()))
;; car 异常边界测试
(check-catch 'wrong-type-arg (car 123))
(check-catch 'wrong-type-arg (car "hello"))
(check-catch 'wrong-type-arg (car #t))
(check-catch 'wrong-type-arg (car #\a))
(check-catch 'wrong-type-arg (car #(a b c)))
(check-catch 'wrong-number-of-args (car))
(check-catch 'wrong-number-of-args (car '(1 2) '(3 4)))
;; 补充边界条件测试 - 完善car边界条件
;; 各种数据结构边界测试
(check (car '(symbol)) => 'symbol)
(check (car '(#t #f)) => #t)
(check (car '(42 24)) => 42)
(check (car '(3.14 2.71)) => 3.14)
(check (car '(1/2 2/3)) => 1/2)
(check (car '(1.0+2.0i 3.0+4.0i)) => 1.0+2.0i)
(check (car '(#	ab #\newline)) => #	ab)
;; 嵌套结构和特殊边界值测试
(check (car '((a (b (c))))) => '(a (b (c))))
(check (car '(((1 2) 3) 4)) => '((1 2) 3))
(check (car '(() b c)) => '())
(check (car '('(a b) '(c d))) => ''(a b))
(check (car '('(a b) '(c d))) => ''(a b))
;; 向量和字节向量作为car值测试
(check (car '(#(1 2 3) #(4 5))) => #(1 2 3))
(check (car '(#u(255 128) #u(1 2))) => #u(255 128))
;; Scheme符号和过程边界测试
(check (car '(procedure? symbol?)) => 'procedure?)
(check (car '(+ - * /)) => '+)
(check (car '(sqrt abs)) => 'sqrt)
;; 连续空列表嵌套边界测试
(check (car '((((()))))) => '(((()))))
(check (car '((a (()) b) c)) => '(a (()) b))
;; Unicode和特殊字符边界测试
(check (car '("中文" "world")) => "中文")
(check (car '("🙂" "🚀")) => "🙂")
(check (car '((list 'a 'b) 'c)) => '(list 'a 'b))
;; 函数和过程对象作为car值测试
(check (car '((lambda (x) (* x x)) (lambda (y) (+ y 1))))
  =>
  '(lambda (x) (* x x))
) ;check
;; 极端边界：现存表达式嵌套
(check (car '((begin 1 2 3) (begin 4 5))) => '(begin 1 2 3))
(check (car '((let ((x 10)) x) (let ((y 20)) y)))
  =>
  '(let ((x 10)) x)
) ;check
;; 确保对car函数的精确数据类型边界验证
(check (car '((define f (lambda (x) x)) (define g (lambda (x) x))))
  =>
  '(define f (lambda (x) x))
) ;check
(check-catch 'wrong-type-arg (car #f))
(check-catch 'wrong-type-arg (car '[]))
(check-catch 'wrong-type-arg (car '()))
(check-catch 'wrong-number-of-args (car 42 84))
(check-catch 'wrong-type-arg (car '*))
(check-report)