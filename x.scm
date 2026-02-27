; (display (sc-expand '(define-library (x)
;                        (import (liii error)
;                                (liii base))
;                        (begin
;                          (newline)))
;                     #f '(L C) '(L)))

(define-library (x)
  (export (rename iota iota))
  (begin (newline)))

(define-library (x y))
