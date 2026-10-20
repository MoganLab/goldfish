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
    (liii pi blueprint args)
    (liii pi compass args)
    (liii pi modes)
    (liii pi help)
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

    (define (%run-pi-main parsed)
      (let-values (((initial-msg rest-msgs)
                    (build-initial-prompt (and (not (stdin-tty?)) (read-all-stdin))
                      (process-file-arguments (pi-args-ref parsed 'file-args))
                      (pi-args-ref parsed 'messages)
                    ) ;build-initial-prompt
                   ) ;
                  ) ;
        (pi-validate-review-flags parsed)
        (when (and (pi-args-ref parsed 'red-green)
                (string-null? initial-msg)
                (null? rest-msgs)
              ) ;and
          (error 'pi-arg-error
            "--red-green requires a task description (message text or @file)"
          ) ;error
        ) ;when
        (when (and (pi-args-ref parsed 'blueprint)
                (string-null? initial-msg)
                (null? rest-msgs)
              ) ;and
          (error 'pi-arg-error
            "--blueprint requires an objective (message text or @file)"
          ) ;error
        ) ;when
        (cond ((pi-args-ref parsed 'help) (display-pi-help) 0)
              ((pi-args-ref parsed 'list-models) (%handle-list-models parsed))
              ((and (string-null? initial-msg)
                 (null? rest-msgs)
                 (not (pi-args-ref parsed 'continue))
                 (not (pi-args-ref parsed 'compact))
                 (not (pi-args-ref parsed 'code-review))
                 (not (pi-args-ref parsed 'simplify))
                 (not (pi-args-ref parsed 'red-green))
                 (not (pi-blueprint-flag? parsed))
                 (not (pi-compass-flag? parsed))
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
      ) ;let-values
    ) ;define

    (define (%format-catch-error err-type info)
      (cond
       ((and (pair? info) (string? (car info))) (car info))
       ((and (pair? info) (pair? (car info)) (string? (caar info))) (caar info))
       (else (object->string (cons err-type info)))
      ) ;cond
    ) ;define

    (define (run-pi raw-args)
      (catch #t
        (lambda () (%run-pi-main (parse-pi-args (strip-command-prefix raw-args))))
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
