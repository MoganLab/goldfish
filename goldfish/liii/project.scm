(define-library (liii project)
  (import (scheme base) (liii base) (liii path))
  (export project-root gfproject-load-config function-libraries)
  (begin
    (define (project-root)
      (let loop ((dir (g_getcwd)))
        (cond ((or (not dir) (not (string? dir)) (string=? dir "")) #f)
              ((g_isfile (path->string (path-join (path dir) "gfproject.json"))) dir)
              (else
                (let* ((p (path dir))
                       (parent (path->string (path-parent p)))
                      ) ;
                  (if (or (string=? parent dir) (string=? parent "") (string=? parent "/"))
                    #f
                    (loop parent)
                  ) ;if
                ) ;let*
              ) ;else
        ) ;cond
      ) ;let
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
