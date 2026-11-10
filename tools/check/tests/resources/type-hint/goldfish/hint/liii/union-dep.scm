(define-library (hint liii union-dep)
  (import (scheme base))
  (export hint-liii/union-dep)
  (begin

    ;; my-take : ((or #f string?) -> any?)
    (define hint-my-take '(((or #f string?) -> any?)))

    ;; my-opts : (boolean? boolean? -> any?)
    (define hint-my-opts '((boolean? boolean? -> any?)))

    (define (hint-liii/union-dep name)
      (case name
       ((my-take) hint-my-take)
       ((my-opts) hint-my-opts)
       (else #f)
      ) ;case
    ) ;define
  ) ;begin
) ;define-library
