(import (goldfish))
(begin-for-syntax
  (begin-for-syntax
    (define (d2 x) (* x 2)))
  (define-syntax at2
    (lambda (stx)
      (syntax-case stx ()
        ((_) (datum->syntax stx (d2 10)))))))
(define w (at2))
(write w)
(newline)
