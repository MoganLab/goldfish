(import (liii check))
(check-set-mode! 'report-failed)
(check (let ()
         (define-constant PI 3.14159)
         PI
       ) ;let
  =>
  3.14159
) ;check
(check (let ()
         (define-constant GREETING "Hello")
         GREETING
       ) ;let
  =>
  "Hello"
) ;check
(check (let ()
         (define-constant ANSWER 42)
         ANSWER
       ) ;let
  =>
  42
) ;check
(check (let ()
         (define-constant (square x) (* x x))
         (square 5)
       ) ;let
  =>
  25
) ;check
(check (let ()
         (define-constant (add x y) (+ x y))
         (add 3 4)
       ) ;let
  =>
  7
) ;check
(check-catch 'immutable-error
  (let ()
    (define-constant X 1)
    (set! X 2)
  ) ;let
) ;check-catch
(check-catch 'immutable-error
  (let ()
    (define-constant Y "test")
    (set! Y "new")
  ) ;let
) ;check-catch
(check-catch 'immutable-error
  (let ()
    (define-constant (func x) x)
    (set! func (lambda (x) (+ x 1)))
  ) ;let
) ;check-catch
(check (let ()
         (define-constant TEST-CONST 123)
         (constant? 'TEST-CONST)
       ) ;let
  =>
  #t
) ;check
(check (let ()
         (define TEST-VAR 456)
         (constant? 'TEST-VAR)
       ) ;let
  =>
  #f
) ;check
(check-catch 'syntax-error
  (define-constant)
) ;check-catch
(check-catch 'syntax-error
  (define-constant NAME)
) ;check-catch
(check (let ()
         (define-constant (factorial n)
           (if (<= n 1)
             1
             (* n (factorial (- n 1)))
           ) ;if
         ) ;define-constant
         (factorial 5)
       ) ;let
  =>
  120
) ;check
(check (let ()
         (define-constant (make-adder x)
           (lambda (y) (+ x y))
         ) ;define-constant
         ((make-adder 10) 5)
       ) ;let
  =>
  15
) ;check
(check (let ((x 1))
         (define-constant y 2)
         (+ x y)
       ) ;let
  =>
  3
) ;check
(check-report)