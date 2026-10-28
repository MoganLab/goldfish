(define-library (hint srfi srfi-1)
  (import (scheme base))
  (export hint-srfi/srfi-1)
  (begin

    (define hint-e2e-shared-fn '((any? -> any?)))

    (define (hint-srfi/srfi-1 name)
      (case name
       ((e2e-shared-fn) hint-e2e-shared-fn)
       (else #f)
      ) ;case
    ) ;define
  ) ;begin
) ;define-library
