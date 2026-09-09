(import (goldfish))
(begin-for-syntax
  (begin-for-syntax
    (define (deep x) (* x 7))
    (define-syntax at2
      (lambda (stx)
        (syntax-case stx ()
        ((_) (datum->syntax stx (deep 10))))))))
(define w (at2))
(write w)
(newline)
