(define-library (goldfish repro-hygiene)
  (import (goldfish))
  (import (scheme base))
  (export gen-defval
    gen-defval2
    gen-let)
  (define-syntax gen-defval
    (lambda (stx)
      (datum->syntax stx '(define-values (a b) (values 1 2)))))
  (define-syntax gen-defval2
    (lambda (stx)
      (datum->syntax (quote-syntax define-values)
                     '(define-values (c d) (values 3 4)))))
  (define-syntax gen-let
    (lambda (stx)
      (datum->syntax stx '(let ((x 1)) x)))))
