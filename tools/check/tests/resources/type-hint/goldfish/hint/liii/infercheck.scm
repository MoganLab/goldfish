(define-library (hint liii infercheck)
  (import (scheme base))
  (export hint-liii/infercheck)
  (begin

    ;; bad-ref : (any? any? -> any?)
    (define hint-bad-ref '((any? any? -> any?)))

    (define (hint-liii/infercheck name)
      (case name
       ((bad-ref) hint-bad-ref)
       (else #f)
      ) ;case
    ) ;define
  ) ;begin
) ;define-library
