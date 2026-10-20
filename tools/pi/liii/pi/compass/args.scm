;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi compass args)
  (import (scheme base) (liii string) (liii list) (liii pi blueprint args))
  (export pi-compass-args-defaults pi-compass-handle-flag pi-compass-validate
    pi-compass-local-flow? pi-compass-flag? pi-compass-tour-topic-arg
  ) ;export
  (begin

    (define (pi-compass-args-defaults)
      (list (cons 'codemap #f)
        (cons 'tour #f)
        (cons 'tour-topic #f)
        (cons 'refresh #f)
        (cons 'codemap-file #f)
        (cons 'no-codemap-inject #f)
      ) ;list
    ) ;define

    (define (pi-compass-handle-flag arg next-arg)
      (cond ((string=? arg "--codemap") (values 'codemap #t 1))
            ((string=? arg "--refresh") (values 'refresh #t 1))
            ((string=? arg "--no-codemap-inject") (values 'no-codemap-inject #t 1))
            ((string-starts? arg "--codemap-file=")
             (values 'codemap-file (substring arg 15 (string-length arg)) 1)
            ) ;
            ((and (string=? arg "--codemap-file") next-arg)
             (values 'codemap-file next-arg 2)
            ) ;
            ((string=? arg "--tour")
             (if (and next-arg
                   (not (string-starts? next-arg "-"))
                   (not (string-starts? next-arg "@"))
                 ) ;and
               (values 'tour next-arg 2)
               (values 'tour #t 1)
             ) ;if
            ) ;
            (else (values #f #f 0))
      ) ;cond
    ) ;define

    (define (%ref parsed key)
      (let ((pair (assoc key parsed)))
        (if pair (cdr pair) #f)
      ) ;let
    ) ;define

    (define (pi-compass-flag? parsed)
      (or (%ref parsed 'codemap) (%ref parsed 'tour))
    ) ;define

    (define (pi-compass-local-flow? parsed)
      (or (%ref parsed 'codemap) (%ref parsed 'tour))
    ) ;define

    ;; --tour 的主题：解析时把其后第一个非 - / @ 参数存进 tour 键。
    (define (pi-compass-tour-topic-arg parsed)
      (let ((v (%ref parsed 'tour)))
        (if (string? v) v #f)
      ) ;let
    ) ;define

    (define (%workflow-flag? parsed)
      (or (%ref parsed 'code-review)
        (%ref parsed 'simplify)
        (%ref parsed 'red-green)
        (pi-blueprint-flag? parsed)
      ) ;or
    ) ;define

    (define (pi-compass-validate parsed)
      (let ((codemap (%ref parsed 'codemap))
            (tour (%ref parsed 'tour))
            (refresh (%ref parsed 'refresh))
            (codemap-file (%ref parsed 'codemap-file))
           ) ;
        (when (and codemap tour)
          (error 'pi-arg-error "--codemap and --tour cannot be used together")
        ) ;when
        (when (and (or codemap tour) (%workflow-flag? parsed))
          (error 'pi-arg-error
            "--codemap and --tour cannot be used with --code-review, --simplify, --red-green, --blueprint or --plan-*"
          ) ;error
        ) ;when
        (when (and refresh (not codemap))
          (error 'pi-arg-error "--refresh requires --codemap")
        ) ;when
        (when (and codemap-file (not codemap))
          (error 'pi-arg-error "--codemap-file requires --codemap")
        ) ;when
        #t
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
