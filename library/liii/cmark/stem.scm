
(define-library (liii cmark stem)
  (import (scheme base))
  (import (liii cmark stem util))
  (import (liii cmark stem blocks))
  (export markdown->md-stem)
  (begin
    (define (markdown->md-stem markdown)
      (cons 'md-stem (parse-markdown-blocks (string-split-lines markdown)))
    ) ;define
  ) ;begin
) ;define-library
