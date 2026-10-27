(define-library (hint scheme base)
  (import (scheme base))
  (export hint-scheme/base)
  (begin

    (define hint-cadr '((pair? -> any?)))

    (define (hint-scheme/base name)
      (case name
       ((cadr) hint-cadr)
       (else #f)
      ) ;case
    ) ;define
  ) ;begin
) ;define-library
