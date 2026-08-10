;;;; r7rs-small.scm
;;; A self-contained r7rs-small example, run through the self-hosted
;;; pipeline:
;;;
;;;   blue run examples/r7rs-small.scm
;;;
;;; which loads seed -> kernel artifact (expander + evaluator) -> lib layer
;;; -> standard library, then interprets the program with the self-hosted
;;; evaluator (self-run-program), not the host's eval.
;;;
;;; Exercises the r7rs-small surface: (scheme base) import, define-library,
;;; syntax-rules, define-record-type, let-values / let*-values, do, case,
;;; cond with =>, when / unless, delay / force, tail recursion, and the
;;; procedure set (lists, vectors, strings, chars, bytevectors).

(import (scheme base))
(import (scheme char))

;;; --- a user library (define-library / import / export) ---

(define-library (examples counters)
  (export make-counter counter-add counter-value)
  (import (scheme base))
  (define-record-type <counter>
    (make-counter value)
    counter?
    (value counter-value set-counter-value!))
  (define (counter-add c n)
    (set-counter-value! c (+ (counter-value c) n))
    (counter-value c)))

(import (examples counters))

;;; --- a syntax-rules macro ---

(define-syntax swap!
  (syntax-rules ()
    ((swap! a b)
     (let ([tmp a])
       (set! a b)
       (set! b tmp)))))

;;; --- let-values / let*-values (multiple values) ---

(let-values ([(q r) (floor/ 17 5)])
  (display "17 = 5*") (display q) (display " + ") (display r) (newline))

(let*-values ([(a b) (values 3 4)]
              [(c) (values (+ a b))])
  (display "let*-values: ") (display c) (newline))

;;; --- do loop ---

(display "sum 1..10 = ")
(display (do ([i 1 (+ i 1)]
              [acc 0 (+ acc i)])
             ((> i 10) acc)))
(newline)

;;; --- case ---

(display "case 3 -> ")
(display (case 3 ((1 2) 'small) ((3 4) 'medium) (else 'large)))
(newline)

;;; --- cond with => ---

(display "cond => ")
(display (cond ((assoc 'b '((a . 1) (b . 2))) => cdr) (else 'none)))
(newline)

;;; --- when / unless ---

(when #t (display "when ran")(newline))
(unless #f (display "unless ran")(newline))

;;; --- delay / force: lazy, evaluated once, cached ---

(define lazy-sum
  (delay (begin (display "computing lazy sum...") (newline)
                (+ 1 2 3 4 5))))
(display "first force: ") (display (force lazy-sum)) (newline)
(display "second force (cached, no recompute): ") (display (force lazy-sum)) (newline)

;;; --- define-record-type (user space) ---

(define-record-type point
  (make-point x y)
  point?
  (x point-x)
  (y point-y))

(define p (make-point 3 4))
(display "point (+ x y) = ") (display (+ (point-x p) (point-y p))) (newline)

;;; --- the user library in action ---

(define c (make-counter 10))
(display "counter after +5 = ") (display (counter-add c 5)) (newline)

;;; --- the swap! macro (set! on top-level bindings) ---

(define a 1)
(define b 2)
(swap! a b)
(display "swap: ") (display (list a b)) (newline)

;;; --- vectors, strings, chars, bytevectors ---

(display "vector-map: ")
(display (vector->list (vector-map (lambda (x) (* x x)) #(1 2 3))))
(newline)

(display "string-upcase: ") (display (string-upcase "hello")) (newline)
(display "char-upcase: ") (display (char-upcase #\x)) (newline)
(display "string->utf8 -> u8-list: ")
(display (bytevector->u8-list (string->utf8 "hi")))
(newline)

;;; --- higher-order procedures ---

(display "map: ") (display (map (lambda (x) (* x x)) '(1 2 3 4))) (newline)

(define total 0)
(for-each (lambda (x) (set! total (+ total x))) '(10 20 30))
(display "for-each total = ") (display total) (newline)

;;; --- tail recursion ---

(define (fact n) (if (< n 2) 1 (* n (fact (- n 1)))))
(display "fact 10 = ") (display (fact 10)) (newline)
