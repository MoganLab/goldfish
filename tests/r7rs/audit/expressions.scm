(import (scheme base) (scheme eval) (scheme write) (liii check))
(check-set-mode! 'report-failed)
(include "../fixtures/semantic-audit.scm")
(audit-check 'expressions.lexical
  (lambda () (audit-eval '(let ((x 1)) (let ((f (lambda () x))) (let ((x 2)) (f)))))) 1)
(audit-check 'expressions.literal
  (lambda () (list (audit-eval '(quote (a . b))) (audit-eval '#(1 a)) (audit-eval "λ") (audit-eval #t)))
  '((a . b) #(1 a) "λ" #t))
(audit-check 'expressions.application-once
  (lambda () (audit-eval '(let ((n 0))
                           ((begin (set! n (+ n 1)) (lambda (a b) n))
                            (begin (set! n (+ n 1)) 1) (begin (set! n (+ n 1)) 2))))) 3)
(audit-check 'expressions.formals
  (lambda () (audit-eval '(list ((lambda (a b . rest) (list a b rest)) 1 2 3 4)
                                ((lambda args args) 1 2 3)))) '((1 2 (3 4)) (1 2 3)))
(audit-check 'expressions.conditional
  (lambda () (audit-eval '(list (if #f 'yes 'no) (if 0 'yes 'no) (if '() 'yes 'no)))) '(no yes yes))
(audit-check 'expressions.assignment
  (lambda () (audit-eval '(let ((x 1)) (let ((read-x (lambda () x))) (set! x 42) (read-x))))) 42)
(audit-check 'expressions.parallel-let
  (lambda () (audit-eval '(let ((x 10)) (list (let ((x 1) (y x)) y) (let* ((x 1) (y x)) y))))) '(10 1))
(audit-check 'expressions.letrec
  (lambda () (audit-eval '(letrec ((f (lambda () (g))) (g (lambda () 42))) (f)))) 42)
(audit-check 'expressions.let-values
  (lambda () (audit-eval '(let-values (((a b) (values 1 2)) ((c . rest) (values 3 4 5))) (list a b c rest)))) '(1 2 3 (4 5)))
(audit-check 'expressions.sequencing
  (lambda () (audit-eval '(let ((x 0)) (begin (set! x 1) (set! x (+ x 2)) x)))) 3)
(audit-check 'expressions.iteration
  (lambda () (audit-eval '(do ((i 0 (+ i 1)) (total 0 (+ total i))) ((= i 4) total)))) 6)
(audit-check 'expressions.cond-arrow
  (lambda () (audit-eval '(cond ((assv 'b '((a . 1) (b . 42))) => cdr) (else #f)))) 42)
(check-report)
