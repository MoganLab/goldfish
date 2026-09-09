(import (goldfish))
(begin-for-syntax
  (define (deep x) (* x 100))
  (begin-for-syntax
    (define (deep x) (* x 7))
    (define-syntax at2
      (lambda (stx)
        (syntax-case stx ()
          ((_) (datum->syntax stx (deep 3)))))))
  (define-syntax ph1use
    (lambda (stx)
      (syntax-case stx ()
      ((_) (datum->syntax stx (deep 2)))))))
(define value (at2))
(define v3 (ph1use))
(write (list value v3))
(newline)
