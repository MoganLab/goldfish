(import (scheme base) (scheme eval) (scheme write) (liii check))
(check-set-mode! 'report-failed)
(include "../fixtures/semantic-audit.scm")
(audit-check 'equivalence.immediate (lambda () (list (eqv? #t #t) (eqv? #t #f) (eqv? 'a 'a)
                                                   (eqv? 'a 'b) (eqv? '() '()) (eqv? #\λ #\λ))) '(#t #f #t #f #t #t))
(audit-check 'equivalence.exactness (lambda () (list (eqv? 2 2.0) (equal? 2 2.0) (eqv? 1/2 2/4)
                                                   (eqv? (expt 2 200) (* 2 (expt 2 199))))) '(#f #f #t #t))
(audit-check 'equivalence.nonempty-locations
  (lambda () (let ((p (cons 1 2)) (s (string #\a)) (v (vector 1)) (b (bytevector 1)))
               (list (eq? p p) (eqv? p (cons 1 2)) (eq? s s) (eqv? s (string #\a))
                     (eq? v v) (eqv? v (vector 1)) (eq? b b) (eqv? b (bytevector 1)))))
  '(#t #f #t #f #t #f #t #f))
(audit-check 'equivalence.deep (lambda () (equal? (list (vector "λ" (bytevector 0 255)))
                                                 (list (vector (string #\λ) (bytevector 0 255))))) #t)
(audit-check 'equivalence.procedure-identity (lambda () (let ((p (lambda (x) x))) (list (eq? p p) (eqv? p p) (equal? p p)))) '(#t #t #t))
(audit-check 'equivalence.distinct-state
  (lambda () (let ((make (lambda () (let ((n 0)) (lambda () (set! n (+ n 1)) n)))))
               (eqv? (make) (make)))) #f)
(audit-check 'equivalence.nan-boundary (lambda () (list (eqv? 0.0 +nan.0) (boolean? (eqv? +nan.0 +nan.0)))) '(#f #t))
(audit-check 'equivalence.signed-zero
  (lambda () (if (= (/ 1.0 0.0) (/ 1.0 -0.0)) #t (not (eqv? 0.0 -0.0)))) #t)
(audit-check 'equivalence.sharing
  (lambda () (let ((x (list 1 2))) (equal? (list x x) (list (list 1 2) (list 1 2))))) #t)
(audit-check 'equivalence.cyclic-unfolding
  (lambda () (let ((x (list 'a 'b)) (y (list 'a 'b 'a 'b)))
               (set-cdr! (cdr x) x) (set-cdr! (cdddr y) y) (equal? x y))) #t)
(audit-check 'equivalence.cyclic-mismatch
  (lambda () (let ((x (list 'a)) (y (list 'b))) (set-cdr! x x) (set-cdr! y y) (equal? x y))) #f)
(audit-check 'equivalence.unspecified
  (lambda () (list (boolean? (eq? 2 2)) (boolean? (eqv? "" ""))
                   (boolean? (eqv? (lambda () 1) (lambda () 1))))) '(#t #t #t))
(check-report)
