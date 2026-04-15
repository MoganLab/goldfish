(import (liii check))
(import (liii base))
(import (scheme base))
(check-set-mode! 'report-failed)
;; boolean=?
;; 比较两个或多个布尔值是否相等。
;;
;; 语法
;; ----
;; (boolean=? bool1 bool2 . more-bools)
;;
;; 参数
;; ----
;; bool1, bool2, ... : boolean?
;; 布尔值，可以是 #t (真) 或 #f (假)。
;;
;; 返回值
;; ------
;; boolean?
;; 如果所有给定的布尔值都相等，则返回 #t (真)，否则返回 #f (假)。
;;
;; 说明
;; ----
;; 1. 至少需要两个参数
;; 2. 所有参数必须都是布尔值 (#t 或 #f)
;; 3. 当所有布尔值相同时返回 #t，否则返回 #f
;; 4. 支持比较两个或多个布尔值
;;
;; 错误处理
;; --------
;; wrong-number-of-args
;; 当参数数量少于2个时抛出错误。
;; boolean=? 基本测试
(check (boolean=? #t #t) => #t)
(check (boolean=? #f #f) => #t)
(check (boolean=? #t #f) => #f)
(check (boolean=? #f #t) => #f)
(check (boolean=? 1 #t) => #f)
;; 多参数测试
(check (boolean=? #t #t #t) => #t)
(check (boolean=? #f #f #f) => #t)
(check (boolean=? #t #t #f) => #f)
(check (boolean=? #t #f #t) => #f)
(check (boolean=? #f #t #t) => #f)
;; 边界测试
(check (boolean=? #t #t #t #t #t) => #t)
(check (boolean=? #f #f #f #f #f) => #t)
(check (boolean=? #t #t #f #t #t) => #f)
;; 错误处理测试
(check-catch 'wrong-number-of-args (boolean=?))
(check-catch 'wrong-number-of-args (boolean=? #t))
(check (apply + (list 3 4)) => 7)
(check (apply + (list 2 3 4)) => 9)
(check (values 4) => 4)
(check (values) => #<unspecified>)
(check (+ (values 1 2 3) 4) => 10)
(check (string-ref ((lambda () (values "abcd" 2)))) => #\c)
(check (+ (call/cc (lambda (ret) (ret 1 2 3))) 4) => 10)
(check (call-with-values (lambda () (values 4 5)) (lambda (x y) x))
  =>
  4
) ;check
(check (*) => 1)
(check (call-with-values * -) => -1)
(check (receive (a b) (values 1 2) (+ a b)) => 3)
(guard (condition (else (display "condition: ")
                    (write condition)
                    (newline)
                    'exception
                  ) ;else
       ) ;condition
  (+ 1 (raise 'an-error))
) ;guard
(guard (condition (else (display "something went wrong") (newline) 'dont-care)
       ) ;condition
  (+ 1 (raise 'an-error))
) ;guard
(with-input-from-string "(+ 1 2)"
  (lambda ()
    (let ((datum (read)))
      (check-true (list? datum))
      (check datum => '(+ 1 2))
    ) ;let
  ) ;lambda
) ;with-input-from-string
(check (eof-object) => #<eof>)
(check-true ((compose not zero?) 1))
(check-false ((compose not zero?) 0))
(check (let ((x 1)) x) => 1)
(check (let ((x 1)) (+ x 1)) => 2)
(let ((add1/add (lambda* (x (y 1)) (+ x y))))
  (check (add1/add 1) => 2)
  (check (add1/add 0) => 1)
  (check (add1/add 1 2) => 3)
) ;let
(define add3
  (typed-lambda ((i integer?) (x real?) z) (+ i x z))
) ;define
(check (add3 1 2 3) => 6)
(check-catch 'type-error (add3 1.2 2 3))
(check-report)