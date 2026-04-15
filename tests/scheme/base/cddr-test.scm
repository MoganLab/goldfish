(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
(check (cddr '(a b c . d)) => '(c . d))
(check (cddr '(a b c)) => '(c))
(check (cddr '(1 2 . 3)) => 3)
(check (cddr '((a b) c . d)) => 'd)
(check (cddr (cons 'a (cons 'b (cons 'c 'd))))
  =>
  '(c . d)
) ;check
(check-catch 'wrong-type-arg (cddr '()))
(check-catch 'wrong-type-arg (cddr 123))
(check-catch 'wrong-type-arg
  (cddr "hello")
) ;check-catch
(check-catch 'wrong-type-arg (cddr #t))
(check-catch 'wrong-number-of-args
  (cddr)
) ;check-catch
(check-catch 'wrong-number-of-args
  (cddr '(1 2) '(3 4))
) ;check-catch
;; cddr边界条件测试补�
(check (cddr '(a b)) => '())
(check (cddr '(1 2)) => '())
(check (cddr '(#t #f)) => '())
(check (cddr '("hello" "world")) => '())
(check (cddr '(() b c)) => '(c))
;; 各种数据类型cddr边界测试
(check (cddr '(123 "text" symbol))
  =>
  '(symbol)
) ;check
(check (cddr '(#ewline #ab #\space))
  =>
  '(#\space)
) ;check
(check (cddr '((a b) c d)) => '(d))
(check (cddr '(#(1 2) #(3 4) #(5 6)))
  =>
  '(#(5 6))
) ;check
(check (cddr '(+ - * /)) => '(* /))
(check (cddr '('(a b) '(c d) '(e f)))
  =>
  '('(e f))
) ;check
;; 极端边界条件测试
(check (cddr '((lambda (x) x) (lambda (y) y) (lambda (z) z))
       ) ;cddr
  =>
  '((lambda (z) z))
) ;check
(check (cddr '((begin 1 2 3) (begin 4 5) (begin 6 7))
       ) ;cddr
  =>
  '((begin 6 7))
) ;check
(check (cddr '(a b c.d)) => '(c.d))
(check (cddr '("中文" "测试" "程序"))
  =>
  '("程序")
) ;check
(check-report)