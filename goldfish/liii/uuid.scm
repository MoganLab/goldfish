(define-library (liii uuid)
  (import (goldfish))
  (export uuid4)
  (begin

    (define (uuid4)
      (g_uuid4)
    ) ;define

  ) ;begin
) ;define-library
