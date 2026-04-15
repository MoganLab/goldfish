(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)

;;
;; car
;; car 是 Scheme 内置函数，用于获取序对的第一个元素（第一个分量）。该函数是 R7RS 标准的基本列表操作函数之一。
;;
;; 语法
;; ----
;; (car pair)
;;
;; 参数
;; ----
;; pair : pair?
;; 可以是序对（即非空列表或显式点对），不能是空列表或其他对象。
;;
;; 返回值
;; ------
;; 任意类型
;; 返回序对的第一个元素（car部分）。根据不同的序对内容，返回类型可以是
;; 符号、数字、列表、点对等任何对象。
;;
;; 说明
;; ----
;; 1. car 是 pair? 谓词的基本操作之一，与 cdr 配合使用处理序对数据
;; 2. 当应用于列表时，返回列表的第一个元素
;; 3. 适用于所有序对数据：不论是点对 (a . b) 还是非空列表 (a b c ...)
;;
;; 错误处理
;; --------
;; wrong-type-arg
;; 当参数不是序对（如空列表 '()、数字、字符串等）时抛出错误。
;;

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
(check (car '(123 "text" symbol))
  =>
  123
) ;check
(check (car '(#\a #\b #\c)) => #\a)
(check (car '((a b) c d)) => '(a b))
(check (car '((((a))))) => '(((a))))
(check (car '("nested" (list) "structure"))
  =>
  "nested"
) ;check
;; 点对结构boundary测试
(check (car '(a . b)) => 'a)
(check (car (cons 1 2)) => 1)
(check (car (cons 'a (cons 'b 'c)))
  =>
  'a
) ;check
(check-catch 'wrong-type-arg (car '()))
;; car 异常边界测试
(check-catch 'wrong-type-arg (car 123))
(check-catch 'wrong-type-arg
  (car "hello")
) ;check-catch
(check-catch 'wrong-type-arg (car #t))
(check-catch 'wrong-type-arg (car #\a))
(check-catch 'wrong-type-arg
  (car #(a b c))
) ;check-catch
(check-catch 'wrong-number-of-args
  (car)
) ;check-catch
(check-catch 'wrong-number-of-args
  (car '(1 2) '(3 4))
) ;check-catch
;; 补充边界条件测试 - 完善car边界条件
;; 各种数据结构边界测试
(check (car '(symbol)) => 'symbol)
(check (car '(#t #f)) => #t)
(check (car '(42 24)) => 42)
(check (car '(3.14 2.71)) => 3.14)
(check (car '(1/2 2/3)) => 1/2)
(check (car '(1.0+2.0i 3.0+4.0i))
  =>
  1.0+2.0i
) ;check
(check (car '(#	ab #\newline)) => #	ab)
;; 嵌套结构和特殊边界值测试
(check (car '((a (b (c)))))
  =>
  '(a (b (c)))
) ;check
(check (car '(((1 2) 3) 4))
  =>
  '((1 2) 3)
) ;check
(check (car '(() b c)) => '())
(check (car '('(a b) '(c d)))
  =>
  ''(a b)
) ;check
(check (car '('(a b) '(c d)))
  =>
  ''(a b)
) ;check
;; 向量和字节向量作为car值测试
(check (car '(#(1 2 3) #(4 5)))
  =>
  #(1 2 3)
) ;check
(check (car '(#u(255 128) #u(1 2)))
  =>
  #u(255 128)
) ;check
;; Scheme符号和过程边界测试
(check (car '(procedure? symbol?))
  =>
  'procedure?
) ;check
(check (car '(+ - * /)) => '+)
(check (car '(sqrt abs)) => 'sqrt)
;; 连续空列表嵌套边界测试
(check (car '((((()))))) => '(((()))))
(check (car '((a (()) b) c))
  =>
  '(a (()) b)
) ;check
;; Unicode和特殊字符边界测试
(check (car '("中文" "world"))
  =>
  "中文"
) ;check
(check (car '("🙂" "🚀")) => "🙂")
(check (car '((list 'a 'b) 'c))
  =>
  '(list 'a 'b)
) ;check
;; 函数和过程对象作为car值测试
(check (car '((lambda (x) (* x x)) (lambda (y) (+ y 1)))
       ) ;car
  =>
  '(lambda (x) (* x x))
) ;check
;; 极端边界：现存表达式嵌套
(check (car '((begin 1 2 3) (begin 4 5)))
  =>
  '(begin 1 2 3)
) ;check
(check (car '((let ((x 10)) x) (let ((y 20)) y))
       ) ;car
  =>
  '(let ((x 10)) x)
) ;check
;; 确保对car函数的精确数据类型边界验证
(check (car '((define f (lambda (x) x)) (define g (lambda (x) x)))
       ) ;car
  =>
  '(define f (lambda (x) x))
) ;check
(check-catch 'wrong-type-arg (car #f))
(check-catch 'wrong-type-arg (car '[]))
(check-catch 'wrong-type-arg (car '()))
(check-catch 'wrong-number-of-args
  (car 42 84)
) ;check-catch
(check-catch 'wrong-type-arg (car '*))
(check-report)