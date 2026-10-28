(define-library (hint liii a7dupx)
  (import (scheme base))
  (export hint-liii/a7dupx)
  (begin

    (define hint-dup-fn '((any? -> any?)))

    (define (hint-liii/a7dupx name)
      (case name
       ((dup-fn) hint-dup-fn)
       (else #f)
      ) ;case
    ) ;define
  ) ;begin
) ;define-library
