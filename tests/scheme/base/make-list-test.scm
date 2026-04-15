(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; make-list 基本功能测试
(check (make-list 0) => '())
(check (make-list 0 'x) => '())
(check (make-list 1) => '(#f))
(check (make-list 1 'singleton)
  =>
  '(singleton)
) ;check
(check (make-list 3) => '(#f #f #f))
(check (make-list 3 'repeat)
  =>
  '(repeat repeat repeat)
) ;check
;; make-list 边界值测试
(check (make-list 10 'a)
  =>
  '(a a a a a a a a a a)
) ;check
(check (make-list 100 'X)
  =>
  (make-list 100 'X)
) ;check
;; make-list 数据类型兼容性测试
(check (make-list 4 42)
  =>
  '(42 42 42 42)
) ;check
(check (make-list 3 3.14)
  =>
  '(3.14 3.14 3.14)
) ;check
(check (make-list 2 #t) => '(#t #t))
(check (make-list 2 #\a) => '(#\a #\a))
(check (make-list 2 "hello")
  =>
  '("hello" "hello")
) ;check
(check (make-list 3 'symbol)
  =>
  '(symbol symbol symbol)
) ;check
(check (make-list 2 #(1 2 3))
  =>
  '(#(1 2 3) #(1 2 3))
) ;check
(check (make-list 2 #u(255 128))
  =>
  '(#u(255 128) #u(255 128))
) ;check
;; make-list 复杂数据类型测试
(check (make-list 3 '()) => '(() () ()))
(check (make-list 2 '(a b c))
  =>
  '((a b c) (a b c))
) ;check
(check (make-list 2 (list 1 2 3))
  =>
  '((1 2 3) (1 2 3))
) ;check
;; make-list 极端边界测试
(check (make-list 2 (cons 'a 'b))
  =>
  '((a . b) (a . b))
) ;check
(check (make-list 1 (make-list 3 'x))
  =>
  '((x x x))
) ;check
(check (make-list 3 (make-list 0))
  =>
  '(() () ())
) ;check
;; make-list 函数和过程对象的基本验证
(check (length (make-list 3 car)) => 3)
(check (list? (make-list 2 car)) => #t)
(check (procedure? (car (make-list 2 car)))
  =>
  #t
) ;check
;; make-list 动态边界验证
(let ((n 5))
  (let ((result (make-list n 'test)))
    (check (length result) => n)
    (check (list? result) => #t)
    (check (car result) => 'test)
  ) ;let
) ;let
;; make-list 内存结构验证
(let ((lst1 (make-list 3 'same))
      (lst2 (make-list 3 'same))
     ) ;
  (check (equal? lst1 lst2) => #t)
  (check (eq? lst1 lst2) => #f)
) ;let
;; 错误参数类型测试
(check-catch 'wrong-number-of-args
  (make-list)
) ;check-catch
(check-catch 'wrong-number-of-args
  (make-list 3 'x 'extra)
) ;check-catch
(check-catch 'out-of-range
  (make-list -1 'x)
) ;check-catch
(check-report)