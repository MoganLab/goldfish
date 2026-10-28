(define-library (hint liii a7builtin)
  (import (scheme base))
  (export hint-liii/a7builtin)
  (begin

    (define hint-builtin-fn '((any? -> any?)))

    (define (hint-liii/a7builtin name)
      (case name
       ((builtin-fn) hint-builtin-fn)
       (else #f)
      ) ;case
    ) ;define
  ) ;begin
) ;define-library
