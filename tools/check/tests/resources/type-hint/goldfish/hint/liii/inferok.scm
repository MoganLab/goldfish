(define-library (hint liii inferok)
  (import (scheme base))
  (export hint-liii/inferok)
  (begin

    ;; good1 : (any? -> any?)
    (define hint-good1 '((any? -> any?)))

    ;; good2 : (any? -> any?)
    (define hint-good2 '((any? -> any?)))

    (define (hint-liii/inferok name)
      (case name
       ((good1) hint-good1)
       ((good2) hint-good2)
       (else #f)
      ) ;case
    ) ;define
  ) ;begin
) ;define-library
