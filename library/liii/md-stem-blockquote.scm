
(define-library (liii md-stem-blockquote)
  (import (scheme base))
  (import (liii markdown-block-collector))
  (export make-blockquote-stem)
  (begin
    (define (make-blockquote-stem lines parse-blocks)
      (cons 'blockquote (parse-blocks (map unquote-blockquote-line lines)))
    ) ;define
  ) ;begin
) ;define-library
