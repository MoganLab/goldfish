(define-library (hint liii numcheck)
  (import (scheme base))
  (export hint-liii/numcheck)
  (begin

    ;; bad-number : (any? -> any?)
    (define hint-bad-number '((any? -> any?)))

    (define (hint-liii/numcheck name)
      (case name
       ((bad-number) hint-bad-number)
       (else #f)
      ) ;case
    ) ;define
  ) ;begin
) ;define-library
