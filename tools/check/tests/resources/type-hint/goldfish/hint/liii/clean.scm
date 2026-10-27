(define-library (hint liii clean)
  (import (scheme base))
  (export hint-liii/clean)
  (begin

    (define hint-f1
      '((any? -> boolean?))
    ) ;define

    (define hint-f2
      '((integer? integer? -> any?)
        (integer? integer? integer? -> any?))
    ) ;define

    (define (hint-liii/clean name)
      (case name
       ((f1) hint-f1)
       ((f2) hint-f2)
       (else #f)
      ) ;case
    ) ;define
  ) ;begin
) ;define-library
