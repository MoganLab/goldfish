;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi cli)
  (import (scheme base)
    (scheme process-context)
    (liii string)
    (liii sys)
    (liii pi)
    (liii pi args)
    (liii pi modes)
    (liii pi help)
    (liii pi extension types)
    (liii pi extension registry)
    (liii pi extension discovery)
    (liii pi extension loader)
  ) ;import
  (export main display-pi-help read-all-stdin run-goldpi run-pi)
  (begin

    (define (read-all-stdin)
      (let loop
        ((acc '()))
        (let ((chunk (read-string 4096 (current-input-port))))
          (if (or (eof-object? chunk) (= (string-length chunk) 0))
            (let ((trimmed (string-trim-both (apply string-append (reverse acc)))))
              (if (string-null? trimmed) #f trimmed)
            ) ;let
            (loop (cons chunk acc))
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    (define (strip-command-prefix args)
      (if (null? args)
        '()
        (let loop
          ((rest (cdr args)))
          (cond ((null? rest) '())
                ((or (string=? (car rest) "pi") (string-ends? (car rest) "/gf")) (cdr rest))
                (else (loop (cdr rest)))
          ) ;cond
        ) ;let
      ) ;if
    ) ;define

    (define (%handle-list-models parsed)
      (let* ((agent-dir (pi-get-agent-dir))
             (cli-provider (pi-args-ref parsed 'provider))
             (target-provider
               (if
                 (and (string? cli-provider)
                   (not (string-null? (string-trim-both cli-provider)))
                 ) ;and
                 cli-provider
                 #f
               ) ;if
             ) ;target-provider
             (models-store (pi-build-models-catalog agent-dir))
             (search-arg (pi-args-ref parsed 'list-models))
             (search-pattern (if (string? search-arg) search-arg #f))
             (rows (pi-list-models models-store target-provider search-pattern))
             (table-text (pi-format-models-table rows search-pattern))
            ) ;
        (display table-text)
        0
      ) ;let*
    ) ;define

    (define (%validate-parsed-args reg parsed initial-msg rest-msgs)
      (let ((validators (extension-registry-validators reg)))
        (for-each
          (lambda (v)
            (catch #t
              (lambda () (v parsed initial-msg rest-msgs))
              (lambda (tag . info) (v parsed))
            ) ;catch
          ) ;lambda
          validators
        ) ;for-each
      ) ;let
    ) ;define

    (define (%has-matching-flow? reg parsed)
      (let loop
        ((flows (extension-registry-flows reg)))
        (cond ((null? flows) #f)
              (((extension-flow-def-predicate (car flows)) parsed) #t)
              (else (loop (cdr flows)))
        ) ;cond
      ) ;let
    ) ;define

    (define (%run-pi-main parsed)
      (let-values (((initial-msg rest-msgs)
                    (build-initial-prompt (and (not (stdin-tty?)) (read-all-stdin))
                      (process-file-arguments (pi-args-ref parsed 'file-args))
                      (pi-args-ref parsed 'messages)
                    ) ;build-initial-prompt
                   ) ;
                  ) ;
        (let ((reg (or (current-extension-registry) (default-extension-registry))))
          (%validate-parsed-args reg parsed initial-msg rest-msgs)
          (cond ((pi-args-ref parsed 'help) (display-pi-help) 0)
                ((pi-args-ref parsed 'list-models) (%handle-list-models parsed))
                ((and (string-null? initial-msg)
                   (null? rest-msgs)
                   (not (pi-args-ref parsed 'continue))
                   (not (pi-args-ref parsed 'compact))
                   (not (%has-matching-flow? reg parsed))
                 ) ;and
                 (display-pi-help)
                 0
                ) ;
                (else
                  (let ((mode (resolve-app-mode (pi-args-ref parsed 'mode)
                                (pi-args-ref parsed 'print)
                                (stdin-tty?)
                                (stdout-tty?)
                              ) ;resolve-app-mode
                        ) ;mode
                       ) ;
                    (dispatch-pi parsed mode initial-msg rest-msgs)
                  ) ;let
                ) ;else
          ) ;cond
        ) ;let
      ) ;let-values
    ) ;define

    (define (%format-catch-error err-type info)
      (cond
       ((and (pair? info) (string? (car info))) (car info))
       ((and (pair? info) (pair? (car info)) (string? (caar info))) (caar info))
       (else (object->string (cons err-type info)))
      ) ;cond
    ) ;define

    (define (%ensure-extensions-loaded!)
      (let* ((reg (default-extension-registry)) (paths (pi-discover-extensions)))
        (pi-load-extensions paths reg)
        reg
      ) ;let*
    ) ;define

    (define (run-pi raw-args)
      (catch #t
        (lambda ()
          (%ensure-extensions-loaded!)
          (%run-pi-main (parse-pi-args (strip-command-prefix raw-args)))
        ) ;lambda
        (lambda (err-type . info)
          (display-stderr (string-append "Error: " (%format-catch-error err-type info)))
          1
        ) ;lambda
      ) ;catch
    ) ;define
    (define run-goldpi run-pi)

    (define (main)
      (exit (run-pi (command-line)))
    ) ;define

  ) ;begin
) ;define-library
