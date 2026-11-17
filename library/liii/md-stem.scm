
(define-library (liii md-stem)
  (import (scheme base))
  (import (liii utils md-stem-util))
  (import (liii md-stem-blocks))
  (export markdown->md-stem)
  (begin
    (define (markdown->md-stem markdown)
      (cons 'md-stem (parse-markdown-blocks (string-split-lines markdown)))
    ) ;define
  ) ;begin
) ;define-library
