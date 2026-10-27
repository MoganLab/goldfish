(define-library (hint liii other)
  (import (scheme base))
  (export hint-liii/other)
  (begin

    (define hint-f1
      '((any? -> any?))
    ) ;define

    (define (hint-liii/other name)
      (case name
       ((f1) hint-f1)
       (else #f)
      ) ;case
    ) ;define
  ) ;begin
) ;define-library
