;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi blueprint args)
  (import (scheme base) (liii string) (liii list))
  (export pi-blueprint-args-defaults pi-blueprint-handle-flag
    pi-blueprint-validate pi-blueprint-local-flow? pi-blueprint-flag?
  ) ;export
  (begin

    (define (pi-blueprint-args-defaults)
      (list (cons 'blueprint #f)
        (cons 'plan-status #f)
        (cons 'plan-next #f)
        (cons 'plan-verify #f)
        (cons 'plan-abandon #f)
        (cons 'approve #f)
        (cons 'blueprint-file #f)
      ) ;list
    ) ;define

    (define (pi-blueprint-handle-flag arg next-arg)
      (cond ((string=? arg "--blueprint") (values 'blueprint #t 1))
            ((string=? arg "--plan-status") (values 'plan-status #t 1))
            ((string=? arg "--plan-next") (values 'plan-next #t 1))
            ((string=? arg "--plan-verify") (values 'plan-verify #t 1))
            ((string=? arg "--plan-abandon") (values 'plan-abandon #t 1))
            ((string=? arg "--approve") (values 'approve #t 1))
            ((string-starts? arg "--blueprint-file=")
             (values 'blueprint-file (substring arg 17 (string-length arg)) 1)
            ) ;
            ((and (string=? arg "--blueprint-file") next-arg)
             (values 'blueprint-file next-arg 2)
            ) ;
            (else (values #f #f 0))
      ) ;cond
    ) ;define

    (define (%ref parsed key)
      (let ((pair (assoc key parsed)))
        (if pair (cdr pair) #f)
      ) ;let
    ) ;define

    (define (%flag-set? parsed key)
      (%ref parsed key)
    ) ;define

    (define (pi-blueprint-flag? parsed)
      (let ((keys '(blueprint plan-status plan-next plan-verify plan-abandon)))
        (any (lambda (k) (%flag-set? parsed k)) keys)
      ) ;let
    ) ;define

    (define (pi-blueprint-local-flow? parsed)
      (or (%flag-set? parsed 'plan-status)
        (%flag-set? parsed 'plan-verify)
        (%flag-set? parsed 'plan-abandon)
      ) ;or
    ) ;define

    (define (pi-blueprint-validate parsed)
      (let* ((keys '(blueprint plan-status plan-next plan-verify plan-abandon))
             (active (filter (lambda (k) (%flag-set? parsed k)) keys))
            ) ;
        (when (> (length active) 1)
          (error 'pi-arg-error
            "Only one of --blueprint, --plan-status, --plan-next, --plan-verify and --plan-abandon can be used at a time"
          ) ;error
        ) ;when
        (when (and (pair? active)
                (or (%ref parsed 'code-review) (%ref parsed 'simplify) (%ref parsed 'red-green))
              ) ;and
          (error 'pi-arg-error
            (string-append "--blueprint and --plan-* cannot be used with "
              "--code-review, --simplify or --red-green"
            ) ;string-append
          ) ;error
        ) ;when
        (when (and (%ref parsed 'approve) (not (%ref parsed 'plan-verify)))
          (error 'pi-arg-error "--approve requires --plan-verify")
        ) ;when
        #t
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
