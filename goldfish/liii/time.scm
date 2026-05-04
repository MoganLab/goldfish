(define-library (liii time)
  (export sleep datetime-now)
  (import (liii base) (scheme time))
  (begin

    (define (sleep seconds)
      (if (not (number? seconds))
        (error 'type-error "(sleep seconds): seconds must be a number")
        (g_sleep seconds)
      ) ;if
    ) ;define

    (define (datetime-now)
      (g_datetime-now)
    ) ;define

  ) ;begin
) ;define-library
