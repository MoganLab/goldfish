(import (liii check)
        (goldfish match))

;; SRFI-262 full feature coverage.

;; ===== quasipatterns (SRFI-262 quasiquote) =====
(check (match '(x 1) (`(x ,n) n) (_ 'fail)) => 1)
(check (match '(y 1) (`(x ,n) n) (_ #f)) => #f)
(check (match 'x (`,v v) (_ #f)) => 'x)
(check (match '(1 2 3) (`(,x 2 3) x) (_ #f)) => 1)
(check (match '(1 2 3) (`(1 (unquote) 2 3) #t) (_ #f)) => #t)
(check (match '(1 2 3) (`((unquote x y z)) (list z y x)) (_ #f)) => '(3 2 1))
(check (match '(1 2 3) (`(1 (unquote-splicing) 2 3) #t) (_ #f)) => #t)
(check (match '(1 2 3) (`(1 ,@x) x) (_ #f)) => '(2 3))
(check (match '(a a b) (`(a ... b) #t) (_ #f)) => #t)
(check (match '(a b c) (`(a . ,x) x) (_ #f)) => '(b c))
(check (match '(a b c) (`(a . ,@x) x) (_ #f)) => '(b c))
(check (match '#(a b c) (`#(a ,@x) x) (_ 'fail)) => '(b c))
(check (let ((c 'not-bound))
         (match `(a `(b ,c))
           (`(a `(b ,c)) c)
           (_ #f)))
       => 'not-bound)
;; (a (... 1 2) b): extended-ellipsis in quasipatterns depends on
;; bounded-repetition NFA support (bounded ... min max), still broken.

;; ===== multi-value binding forms =====
(check (match-let-values (((a b) (values 1 2))) (list b a)) => '(2 1))
(check (match-let*-values (((a b) (values 1 2)) ((c d) (values 3 4)))
         (list d c b a))
       => '(4 3 2 1))
(check (match-values (values 1 2 3) ((a b c) (list c b a))) => '(3 2 1))

(match-define-values (p q) (values 10 20))
(check (list p q) => '(10 20))
(check (match (list 1 (list 2 3)) ((a (b c)) (list a b c)) (_ 'no)) => '(1 2 3))

;; ===== match-let / match-let* / match-letrec =====
(check (match-let (((a b) (list 1 2))) (list b a)) => '(2 1))
(check (match-let* (((a b) (list 1 2)) ((c d) (list 3 4))) (list d c b a))
       => '(4 3 2 1))
(check (match-letrec (((a) (list 1)) ((b) (list 2))) (list a b)) => '(1 2))
(check (match-letrec* (((a) (list 1)) ((b) (list 2))) (list a b)) => '(1 2))

;; ===== seq pattern: ellipsis / bounded ellipsis =====
(define (mk v . xs) (apply list->vector xs v))
(define (len v) (vector-length v))
(define (ref v i) (vector-ref v i))
(check (match (vector 1 2 3)
         ((seq v ((i 0 (+ i 1))) (= i (len v)) (ref v i) a ...) a) (_ 'no))
       => '(1 2 3))
(check (match (vector 'a 'b 'c)
         ((seq v ((i 0 (+ i 1))) (= i (len v)) (ref v i) x ... y) (cons y (length x)))
         (_ 'fail))
       => '(c . 2))
(check (match (vector 'a 'b 'c)
         ((seq v ((i 0 (+ i 1))) (= i (len v)) (ref v i) x y ...) (cons x (length y)))
         (_ 'fail))
       => '(a . 2))
(check (match (vector 'a 'a 'b)
         ((seq v ((i 0 (+ i 1))) (= i (len v)) (ref v i) 'a (... 2) 'b) #t) (_ #f))
       => #t)
(check (match (vector 'a 'a 'a 'b)
         ((seq v ((i 0 (+ i 1))) (= i (len v)) (ref v i) 'a (... 2) 'b) #t) (_ #f))
       => #f)
(check (match (list 1 2 3 4)
         ((seq xs ((i 0 (+ i 1))) (= i (length xs)) (list-ref xs i) a b ...) (list a b)) (_ 'no))
       => '(1 (2 3 4)))

;; ===== list / cons / cons* patterns with ellipsis =====
(check (match '(1 2 3 4) ((list a b c ...) (list a b c)) (_ 'no))
       => '(1 2 (3 4)))
(check (match '(1 2) ((list a ...) a) (_ 'no)) => '(1 2))
(check (match '(1 2 3) ((cons a b) (list a b)) (_ 'no)) => '(1 (2 3)))
(check (match '(1 2 3 4) ((cons* a b c) (list a b c)) (_ 'no)) => '(1 2 (3 4)))
(check (match '(1 2 3 4) ((cons* a b ...) (list a b)) (_ 'no)) => '(1 (2 3 4)))
(check (match '(1 2 3 4) ((cons* a b c ...) (list a b c)) (_ 'no))
       => '(1 2 (3 4)))

;; ===== vector pattern with ellipsis =====
(check (match #(1 2 3) ((vector a b c ...) (list a b c)) (_ 'no))
       => '(1 2 (3)))
(check (match #(1 2 3) ((vector a b c) (list a b c)) (_ 'no)) => '(1 2 3))

;; ===== lset (unordered sequence) =====
(check (match '(1 2 3) ((lset a b c) (list a b c)) (_ 'no)) => '(1 2 3))
(check (match '(1 2 3) ((lset a b c d) 'four) (_ 'no)) => 'no)
(check (match '(2 4 1 3) ((lset a b c ...) (list a b c)) (_ 'no)) => '(2 4 (1 3)))
(check (match '(2 1 3) ((lset 1 2 3) #t) (_ #f)) => #t)
(check (match '(2 1 3) ((lset 1 a b) (list a b)) (_ #f)) => '(2 3))

;; ===== seq/unordered (explicit unordered sequence) =====
(define (mkseq . xs) (list->vector xs))
(check (match (mkseq 2 1 3)
         ((seq/unordered xs ((i 0 (+ i 1))) (= i (vector-length xs))
            (vector-ref xs i) 1 2 3) #t) (_ #f))
       => #t)
(check (match (mkseq 1 2 3)
         ((seq/unordered xs ((i 0 (+ i 1))) (= i (vector-length xs))
            (vector-ref xs i) 1 2 3) #t) (_ #f))
       => #t)
(check (match (mkseq 2 1 3)
         ((seq/unordered xs ((i 0 (+ i 1))) (= i (vector-length xs))
            (vector-ref xs i) 1 x ...) x) (_ #f))
       => '(2 3))
(check (match (mkseq 2 1)
         ((seq/unordered xs ((i 0 (+ i 1))) (= i (vector-length xs))
            (vector-ref xs i) 1 2 a) a) (_ #f))
       => #f)

;; ===== eof-object pattern =====
(check (match (eof-object) ((eof-object) 'eof) (_ 'no)) => 'eof)
(check (match 5 ((eof-object) 'eof) (_ 'no)) => 'no)

;; ===== and / or / not / ? =====
(check (match '(1 2) ((and (? list?) (a b)) (list a b)) (_ 'no)) => '(1 2))
(check (match 'a ((or 'a 'b) 'ab) (_ 'other)) => 'ab)
(check (match 4 ((not (? even?)) 'odd) (_ 'even)) => 'even)
(check (match 5 ((? number? n) n) (_ 'no)) => 5)

;; ===== match reasons =====
(check (match 1 ((2) 'two) ((3) 'three) (_ 'other)) => 'other)
(check (catch #t
         (lambda () (match 1 ((2) 'two) ((3) 'three)))
         (lambda (tag . info)
           (if (match-violation? (car info)) 'violation 'other)))
       => 'other)
(check (match-violation? (make-match-violation)) => #t)

;; ===== if-match =====
(check (if-match (((a b) (list 1 2))) (list 'm a b) 'nomatch) => '(m 1 2))
(check (if-match (((a b) (list 1))) 'm 'nomatch) => 'nomatch)

;; ===== match-ellipsis? =====
(check (match-ellipsis? '...) => #t)
(check (match-ellipsis? '(... 3)) => #t)
(check (match-ellipsis? '(... 1 #t)) => #t)
(check (match-ellipsis? 'x) => #f)

;; ===== custom pattern transformer =====
(define-pattern-syntax pair2
  (lambda (stx)
    (let ((d (syntax->datum stx)))
      (datum->syntax stx (list 'cons (cadr d) (caddr d))))))
(check (match (cons 1 2) ((pair2 a b) (list 'p a b)) (_ 'no)) => '(p 1 2))

;; ===== match-lambda arity grouping =====
(define ml2 (match-lambda ((a b) (list a b)) (_ 'no)))
(check (ml2 1 2) => '(1 2))
(check (ml2 1) => 'no)
(define ml1 (match-lambda ((a) (list 'one a))))
(check (ml1 9) => '(one 9))
(define mlw (match-lambda ((_ a) (list 'got a)) ((b) (list 'single b))))
(check (mlw 'x 9) => '(got 9))
(check (mlw 9) => '(single 9))

;; ===== deep nested patterns =====
(check (match '((1 2) (3 4)) (((a b) (c d)) (list a b c d)) (_ 'no))
       => '(1 2 3 4))
(check (match '#(1 #(2 3) 4) ((vector a (vector b c) d) (list a b c d)) (_ 'no))
       => '(1 2 3 4))

;; ===== reference feature parity =====
(check (match 42 ((? even?) #t) (_ #f)) => #t)
(check (match 21 ((? even?) #t) (_ #f)) => #f)
(check (match 'x ((=> symbol->string "x") #t) (_ #f)) => #t)
(check (match 'y ((=> symbol->string "x") #t) (_ #f)) => #f)
(check (match '(x) ((=> car v) v) (_ 'fail)) => 'x)
(check (match 5 ((=> (lambda (x) (values (quotient x 3) (remainder x 3))) 1 2) #t) (_ #f)) => #t)
(check (match 5 ((=> (lambda (x) (values (quotient x 3) (remainder x 3))) _ 0) #t) (_ #f)) => #f)
(check (match 5 ((=> (lambda (x) (values (quotient x 3) (remainder x 3))) x y) (cons x y)) (_ 'fail)) => '(1 . 2))
(check (match 42 ((and (? exact?) (? even?)) #t) (_ #f)) => #t)
(check (match 42 ((and (? exact?) (? odd?)) #t) (_ #f)) => #f)
(check (match 'a ((and) #t) (_ #f)) => #t)
(check (match 'b ((and 'b) #t) (_ #f)) => #t)
(check (match 1 ((and (? number?) x y) (cons x y)) (_ 'fail)) => '(1 . 1))
(check (match 42 ((or 23 42) #t) (_ #f)) => #t)
(check (match 42 ((or 23 144) #t) (_ #f)) => #f)
(check (match 'a ((or) #t) (_ #f)) => #f)
(check (match 'b ((or 'b) #t) (_ #f)) => #t)
(check (match '(1 2) ((or (list a b) (cons a b)) b) (_ 'fail)) => 2)
(check (match 'a ((not (list)) #t) (_ #f)) => #t)
(check (match '() ((not (list)) #t) (_ #f)) => #f)
(check (match (list->vector '(1 2 3))
              ((seq xs ((idx 0 (+ idx 1)))
                    (= idx (vector-length xs))
                    (vector-ref xs idx)
                    1 2 3) #t)
              (_ #f)) => #t)
(check (match (list->vector '(1 2 3 4))
              ((seq xs ((idx 0 (+ idx 1)))
                    (= idx (vector-length xs))
                    (vector-ref xs idx)
                    1 2 3) #t)
              (_ #f)) => #f)
(check (match (list->vector '(a b c))
              ((seq xs ((idx 0 (+ idx 1)))
                    (= idx (vector-length xs))
                    (vector-ref xs idx)
                    x ...) (length x))
              (_ 'fail)) => 3)
(check (match '((1 2) (3 4) (5 6)) (((a b) ...) (list 'pairs a b)) (_ 'no))
       => '(pairs (1 3 5) (2 4 6)))
(check (match '((1 2) (3 4)) (((a b) ...) (list 'pairs a b)) (_ 'no))
       => '(pairs (1 3) (2 4)))
(check (match (eof-object) ((eof-object) 'eof) (_ 'no)) => 'eof)
(check (match #f ((eof-object) 'eof) (_ 'no)) => 'no)

;; ===== seq* (partial prefix match) =====
;; note: partial requires ref to be safe at terminate?, so iterate
;; curr (termination = exhaustion) instead of indexing by length.
(check (match '(1 2 3 4)
              ((seq* xs ((curr xs (cdr curr)))
                     (not (pair? curr))
                     curr
                     (=> car 1) (=> car 2) (=> car 3))
               #t)
              (_ #f)) => #t)
(check (match '(1 2 3 4)
              ((seq xs ((curr xs (cdr curr)))
                     (not (pair? curr))
                     curr
                     (=> car 1) (=> car 2) (=> car 3))
               #t)
              (_ #f)) => #f)
(check (match '(1 2 3)
              ((seq* xs ((curr xs (cdr curr)))
                     (not (pair? curr))
                     curr
                     (=> car 1) (=> car 2) (=> car 3))
               #t)
              (_ #f)) => #t)
(check (match '(1 2 4)
              ((seq* xs ((curr xs (cdr curr)))
                     (not (pair? curr))
                     curr
                     (=> car 1) (=> car 2) (=> car 3))
               #t)
              (_ #f)) => #f)
(check (match '(1 2 3 4 5)
              ((seq* xs ((curr xs (cdr curr)))
                     (not (pair? curr))
                     curr
                     (=> car a) (=> car b) ... (cons x y))
               (list 'got a b x y))
              (_ 'no)) => '(got 1 (2 3 4) 5 ()))

(check-report)
