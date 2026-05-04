(define-library (liii time)
  (export sleep)
  (import (liii base) (scheme time))
  (begin

    (define (sleep seconds)
      (if (not (number? seconds))
        (error 'type-error "(sleep seconds): seconds must be a number")
        (g_sleep seconds)
      ) ;if
    ) ;define

  ) ;begin
) ;define-library
