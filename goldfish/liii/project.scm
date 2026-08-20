(define-library (liii project)
  (import (scheme base) (liii base) (liii os))
  (export project-root gfproject-load-config function-libraries)
  (begin
    (define (project-root)
      (g_project-root)
    ) ;define
    (define (gfproject-load-config)
      (let ((s (g_gfproject-load-config)))
        (if (string? s) s "{}")
      ) ;let
    ) ;define
    (define (function-libraries name)
      (let ((r (g_function-libraries name)))
        (if (list? r) r '())
      ) ;let
    ) ;define
  ) ;begin
) ;define-library
