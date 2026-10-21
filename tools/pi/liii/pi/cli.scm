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
    (liii pi config)
    (liii pi modes)
    (liii pi help)
    (liii pi tui)
    (liii pi extension types)
    (liii pi extension registry)
    (liii pi extension discovery)
    (liii pi extension loader)
    (liii pi extension install)
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
                 (begin
                   (if (and (stdin-tty?) (stdout-tty?))
                     (let* ((env (get-environment-variables))
                            (base-cfg (resolve-pi-config parsed env))
                            (cfg
                              (let apply-aug
                                ((augs (extension-registry-augmenters reg)) (c base-cfg))
                                (if (null? augs) c (apply-aug (cdr augs) ((car augs) c parsed)))
                              ) ;let
                            ) ;cfg
                           ) ;
                       (pi-app-run (make-pi-app :handler (make-tui-agent-handler parsed cfg)))
                     ) ;let*
                     (display-pi-help)
                   ) ;if
                   0
                 ) ;begin
                ) ;
                (else
                  (let ((mode (resolve-app-mode (pi-args-ref parsed 'mode)
                                (pi-args-ref parsed 'print)
                                (stdin-tty?)
                                (stdout-tty?)
                              ) ;resolve-app-mode
                        ) ;mode
                       ) ;
                    (with-extension-registry reg
                      (lambda () (dispatch-pi parsed mode initial-msg rest-msgs))
                    ) ;with-extension-registry
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
      ;; 内置扩展由 args.scm 的 registry initializer（ensure-builtin-extensions!）负责加载，
      ;; 这里只加载全局/项目级扩展，避免 builtin 目录被重复加载
      (let* ((reg (default-extension-registry))
             (paths (pi-discover-extensions :builtin-dir #f))
            ) ;
        (pi-load-extensions paths reg)
        reg
      ) ;let*
    ) ;define

    (define (%handle-install-command args)
      (let loop
        ((rem args) (local? #f) (url #f))
        (cond
         ((null? rem)
          (if (not url)
            (begin
              (display-stderr "Error: pi install requires a git URL\nUsage: gf pi install [--local|-l] <git-url>\n"
              ) ;display-stderr
              1
            ) ;begin
            (begin
              (let ((installed (pi-install-extension url :local? local?)))
                (display (string-append "Installed extension to " installed "\n"))
                0
              ) ;let
            ) ;begin
          ) ;if
         ) ;
         ((or (string=? (car rem) "--local") (string=? (car rem) "-l"))
          (loop (cdr rem) #t url)
         ) ;
         ((string-starts? (car rem) "-")
          (display-stderr (string-append "Error: unknown install option: " (car rem) "\n")
          ) ;display-stderr
          1
         ) ;
         ((not url) (loop (cdr rem) local? (car rem)))
         (else (display-stderr (string-append "Error: unexpected extra argument: " (car rem) "\n")
               ) ;display-stderr
           1
         ) ;else
        ) ;cond
      ) ;let
    ) ;define

    (define (run-pi raw-args)
      (catch #t
        (lambda ()
          (let ((stripped (strip-command-prefix raw-args)))
            (if (and (pair? stripped) (string=? (car stripped) "install"))
              (%handle-install-command (cdr stripped))
              (begin
                (%ensure-extensions-loaded!)
                (%run-pi-main (parse-pi-args stripped))
              ) ;begin
            ) ;if
          ) ;let
        ) ;lambda
        (lambda (err-type . info)
          (display-stderr (string-append "Error: " (%format-catch-error err-type info) "\n")
          ) ;display-stderr
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
