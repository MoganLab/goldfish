(define-library (hint liii importhint)
  (import (scheme base) (hint scheme base))
  (export hint-liii/importhint)
  (begin

    (define hint-f1
      '((any? -> any?))
    ) ;define

    (define (hint-liii/importhint name)
      (case name
       ((f1) hint-f1)
       (else #f)
      ) ;case
    ) ;define
  ) ;begin
) ;define-library
