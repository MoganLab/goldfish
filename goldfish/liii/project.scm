(define-library (liii project)
  (import (scheme base) (liii base) (liii path) (liii sort) (liii string))
  (export project-root gfproject-load-config function-libraries)
  (begin
    (define (normalize-string s)
      (if (string? s) (string-append s "") s))

    (define (join-path a b)
      (let ([as (normalize-string (if (string? a) a (path->string a)))]
            [bs (normalize-string (if (string? b) b (path->string b)))])
        (cond [(string=? as "") bs]
              [(char=? (string-ref as (- (string-length as) 1)) #\/) (string-append as bs)]
              [else (string-append as "/" bs)])))

    (define (project-root)
      (let loop ([dir (g_getcwd)])
        (cond [(or (not dir) (not (string? dir)) (string=? dir ""))              #f]
              [(g_isfile (normalize-string (join-path dir "gfproject.scm"))) dir]
              [else
                (let* ([p      (path dir)]
                       [parent (path->string (path-parent p))])
                  (if (or (string=? parent dir) (string=? parent "") (string=? parent "/"))
                    #f
                    (loop parent)))])))

    (define (gfproject-load-config)
      (let ([s (g_gfproject-load-config)])
        (if (string? s) s "{}")))

    (define (function-libraries name)
      (let ([r (g_function-libraries name)])
        (if (list? r) r '())))))
