(define-library (liii goldfix-constant)
  (import (scheme base))

  (export LEFT_PAREN)
  (export RIGHT_PAREN)

  (begin
    (define LEFT_PAREN #\()
    (define RIGHT_PAREN #\))
  ) ;begin
) ;define-library
