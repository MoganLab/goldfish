(define-library (liii uuid)
  (export uuid4 uuid7)
  (begin

    (define (uuid4)
      (g_uuid4)
    ) ;define

    (define (uuid7)
      (g_uuid7)
    ) ;define

  ) ;begin
) ;define-library
