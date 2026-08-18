(define-library (liii chez)
  (import (goldfish))
  (export atom?)
  (begin

    (define (atom? x)
      (not (pair? x))
    ) ;define
  ) ;begin
) ;define-library
