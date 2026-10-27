(define-library (hint liii base-fallback)
  (import (scheme base))
  (export hint-liii/base-fallback)
  (begin

    ;; bad-cadr : (any? -> any?)
    (define hint-bad-cadr '((any? -> any?)))

    (define (hint-liii/base-fallback name)
      (case name
       ((bad-cadr) hint-bad-cadr)
       (else #f)
      ) ;case
    ) ;define
  ) ;begin
) ;define-library
