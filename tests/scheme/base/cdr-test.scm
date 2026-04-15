(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
(check (cdr '(a b c . d)) => '(b c . d))
(check (cdr '(a b c)) => '(b c))
(check (cdr '(1 . 2)) => 2)
(check (cdr '((a b) . c)) => 'c)
(check (cdr (cons 1 2)) => 2)
(check (cdr (cons 'a 'b)) => 'b)
(check-catch 'wrong-type-arg (cdr '()))
(check-catch 'wrong-type-arg (cdr 123))
(check-catch 'wrong-type-arg
  (cdr "hello")
) ;check-catch
(check-catch 'wrong-type-arg (cdr #t))
(check-catch 'wrong-number-of-args
  (cdr)
) ;check-catch
(check-catch 'wrong-number-of-args
  (cdr '(1 2) '(3 4))
) ;check-catch
;; cdr边界条件测试补充
(check (cdr '(a)) => '())
(check (cdr '(1)) => '())
(check (cdr '(#t)) => '())
(check (cdr '("hello")) => '())
(check (cdr '(() b c)) => '(b c))
(check (cdr '((a b))) => '())
(check (cdr '((((a))))) => '())
;; 各种数据类型cdr边界测试
(check (cdr '(123 "text" symbol))
  =>
  '("text" symbol)
) ;check
(check (cdr '(#
ewline #	ab #\space))
  =>
  '(#	ab #\space)
) ;check
(check (cdr '((a b) c d)) => '(c d))
(check (cdr '(#(1 2) #(3 4)))
  =>
  '(#(3 4))
) ;check
(check (cdr '(+ - * /)) => '(- * /))
(check (cdr '('(a b) '(c d)))
  =>
  '('(c d))
) ;check
;; 极端边界条件测试
(check (cdr '((lambda (x) x) (lambda (y) y)))
  =>
  '((lambda (y) y))
) ;check
(check (cdr '((begin 1 2 3) (begin 4 5)))
  =>
  '((begin 4 5))
) ;check
(check (cdr '(a b.c d)) => '(b.c d))
(check (cdr '("中文" "测试"))
  =>
  '("测试")
) ;check
(check-report)