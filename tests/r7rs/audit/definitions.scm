(import (scheme base) (scheme eval) (scheme write) (liii check))
(check-set-mode! 'report-failed)
(include "../fixtures/semantic-audit.scm")
(audit-check 'definitions.sequential
  (lambda () (audit-eval '(let () (define x 1) (define y (+ x 2)) (define z (+ y 3)) (list x y z)))) '(1 3 6))
(audit-check 'definitions.whole-region
  (lambda () (audit-eval '(let ((x 'outer)) (let () (define f (lambda () x)) (define x 'inner) (f))))) 'inner)
(audit-check 'definitions.mutual
  (lambda () (audit-eval '(let ()
                           (define (even n) (if (= n 0) #t (odd (- n 1))))
                           (define (odd n) (if (= n 0) #f (even (- n 1))))
                           (list (even 20) (odd 20))))) '(#t #f))
(audit-check 'definitions.begin-splice
  (lambda () (audit-eval '(let () (begin (define x 1) (begin (define y (+ x 1)))) (+ x y)))) 3)
(audit-check 'definitions.macro-splice
  (lambda () (audit-eval '(let ()
                           (define-syntax def (syntax-rules () ((_ name value) (begin (define name value)))))
                           (def x 20) (def y (+ x 2)) (+ x y)))) 42)
(audit-check 'definitions.values
  (lambda () (audit-eval '(let () (define-values (x y) (values 20 22)) (+ x y)))) 42)
(audit-check 'definitions.values-rest
  (lambda () (audit-eval '(let () (define-values (x . rest) (values 1 2 3)) (list x rest)))) '(1 (2 3)))
(audit-check 'definitions.values-all
  (lambda () (audit-eval '(let () (define-values args (values 1 2 3)) args))) '(1 2 3))
(audit-check 'definitions.values-empty
  (lambda () (audit-eval '(let () (define-values () (values)) 42))) 42)
(audit-check 'definitions.record
  (lambda () (audit-eval '(let ()
                           (define-record-type cell (make-cell value) cell? (value cell-ref cell-set!))
                           (let ((c (make-cell 1))) (cell-set! c 42) (list (cell? c) (cell-ref c)))))) '(#t 42))
(check-report)
