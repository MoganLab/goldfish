(define-library (hint liii unioncheck)
  (import (scheme base))
  (export hint-liii/unioncheck)
  (begin

    (define hint-bad-union '((any? -> any?)))
    (define hint-ok-union '((any? -> any?)))
    (define hint-bad-kw '((any? -> any?)))
    (define hint-ok-kw '((any? -> any?)))

    (define (hint-liii/unioncheck name)
      (case name
       ((bad-union) hint-bad-union)
       ((ok-union) hint-ok-union)
       ((bad-kw) hint-bad-kw)
       ((ok-kw) hint-ok-kw)
       (else #f)
      ) ;case
    ) ;define
  ) ;begin
) ;define-library
