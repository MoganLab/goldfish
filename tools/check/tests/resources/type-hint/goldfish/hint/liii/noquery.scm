(define-library (hint liii noquery)
  (import (scheme base))
  (export something-else)
  (begin

    (define hint-f1
      '((any? -> any?))
    ) ;define

    (define (hint-liii/noquery name)
      (case name
       ((f1) hint-f1)
       (else #f)
      ) ;case
    ) ;define
  ) ;begin
) ;define-library
