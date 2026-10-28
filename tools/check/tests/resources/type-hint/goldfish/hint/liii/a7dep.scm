(define-library (hint liii a7dep)
  (import (scheme base))
  (export hint-liii/a7dep)
  (begin

    (define hint-dep-fn '((any? -> any?)))

    (define (hint-liii/a7dep name)
      (case name
       ((dep-fn) hint-dep-fn)
       (else #f)
      ) ;case
    ) ;define
  ) ;begin
) ;define-library
