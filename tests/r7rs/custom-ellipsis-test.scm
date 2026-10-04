(import (scheme base) (scheme eval) (liii check))
(check-set-mode! 'report-failed)

;; The inactive default marker is an ordinary variable, even when repeated.
(define-syntax collect-two
  (syntax-rules ::: () ((_ x ...) (list x ...))))
(check (collect-two 1 2) => '(1 2))
(define-syntax scalar
  (syntax-rules ::: () ((_ ...) ...)))
(check (scalar 42) => 42)
(check (let ((ellipsis-variable 17) (... 42)) (scalar ...)) => 42)
(define-syntax repeated
  (syntax-rules ::: () ((_ ... :::) (list ... :::))))
(check (repeated) => '())
(check (repeated 1 2 3) => '(1 2 3))
(define-syntax nested
  (syntax-rules ::: ()
    ((_ (... :::) :::) (list (list ... :::) :::))))
(check (nested) => '())
(check (nested () (1 2) () (3)) => '(() (1 2) () (3)))
(define-syntax scalar-in-repeat
  (syntax-rules ::: () ((_ ... (x :::)) (list (list ... x) :::))))
(check (scalar-in-repeat 7 ()) => '())
(check (scalar-in-repeat 7 (1 2)) => '((7 1) (7 2)))

;; Vectors and dotted tails retain the same binding and repetition rules.
(define-syntax vector-values
  (syntax-rules ::: () ((_ #(... :::)) #(... :::))))
(check (vector-values #()) => #())
(check (vector-values #(1 2 3)) => #(1 2 3))
(define-syntax dotted
  (syntax-rules ::: () ((_ x . ...) (quote (x . ...)))))
(check (dotted 1 . 2) => '(1 . 2))
(check (dotted 1 2 3) => '(1 2 3))
(define-syntax repeated-tail
  (syntax-rules ::: (end) ((_ x ::: end . ...) (list (list x :::) (quote ...)))))
(check (repeated-tail 1 2 end . 3) => '((1 2) 3))

;; Renaming is per rule; literal identifiers keep their lexical bindings.
(define-syntax alternatives
  (syntax-rules ::: (tag)
    ((_ tag ...) ...)
    ((_ ... :::) (list ... :::))))
(check (alternatives tag 42) => 42)
(check (alternatives 1 2 3) => '(1 2 3))
(define-syntax literal-dots
  (syntax-rules ::: (...)
    ((_ ...) 'matched)
    ((_ x) 'other)))
(check (literal-dots ...) => 'matched)
(check (literal-dots 42) => 'other)
(check (let ((... 42)) (literal-dots ...)) => 'other)
(define-syntax free-dots
  (syntax-rules ::: () ((_ ) (quote ...))))
(check (free-dots) => '...)
(define-syntax explicit-default
  (syntax-rules ... () ((_ x ...) (list x ...))))
(check (explicit-default) => '())
(check (explicit-default 1 2) => '(1 2))
(define-syntax other-marker
  (syntax-rules :: () ((_ ... ::) (list ... ::))))
(check (other-marker 1 2) => '(1 2))

;; Pattern arity and invalid marker errors remain expansion errors.
(define (rejects? datum)
  (guard (exception (else #t))
    (eval datum (environment '(scheme base))) #f))
(check (rejects? '(let-syntax ((m (syntax-rules ::: () ((_ x ...) (list x ...)))))
                    (m 1 2 3))) => #t)
(check (rejects? '(let-syntax ((m (syntax-rules 42 () ((_ x) x)))) (m 1))) => #t)
(define-syntax literal-repeat
  (syntax-rules ::: (...)
    ((_ ... :::) 'matched)))
(check (literal-repeat) => 'matched)
(check (literal-repeat ... ... ...) => 'matched)
(define-syntax literal-marker
  (syntax-rules ::: (:::) ((_ :::) 'matched) ((_ x) 'other)))
(check (literal-marker :::) => 'matched)
(check (literal-marker 42) => 'other)
(check (let ((... 42))
         (let-syntax ((m (syntax-rules ::: () ((_) ...)))) (m))) => 42)
(define-syntax make-collector
  (syntax-rules ::: ()
    ((_ name)
     (define-syntax name
       (syntax-rules () ((_ x ...) (list x ...)))))))
(make-collector generated-collector)
(check (generated-collector 1 2 3) => '(1 2 3))
(check-report)
