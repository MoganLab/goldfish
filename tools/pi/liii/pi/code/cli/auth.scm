;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code cli auth)
  (import (scheme base)
    (scheme write)
    (scheme process-context)
    (liii base)
    (liii string)
    (liii path)
    (liii list)
    (liii error)
    (liii pi ai auth)
    (liii pi code config)
    (liii pi code modes output)
  ) ;import
  (export parse-pi-auth-args pi-auth-print-api-key pi-auth-login run-pi-auth
    pi-auth-args? make-pi-auth-args pi-auth-args-help? pi-auth-args-subcommand
    pi-auth-args-provider pi-auth-args-key
  ) ;export
  (begin

    (define*-record-type <pi-auth-args>
      (make-pi-auth-args (help? #f) (subcommand #f) (provider #f) (key #f))
      pi-auth-args?
      (help? pi-auth-args-help?)
      (subcommand pi-auth-args-subcommand)
      (provider pi-auth-args-provider)
      (key pi-auth-args-key)
    ) ;define*-record-type

    (define (%provider-default-env-var provider)
      (cond ((string=? provider "google") "GEMINI_API_KEY")
            ((string=? provider "kimi-coding") "KIMI_API_KEY")
            ((string=? provider "anthropic") "ANTHROPIC_API_KEY")
            ((string=? provider "glm") "GLM_API_KEY")
            ((string=? provider "deepseek") "DEEPSEEK_API_KEY")
            ((string=? provider "ollama") "OLLAMA_API_KEY")
            ((string=? provider "minimax-cn") "MINIMAX_CN_API_KEY")
            ((or (string=? provider "zai-coding-cn") (string=? provider "zai"))
             "ZAI_CODING_CN_API_KEY"
            ) ;
            ((or (string=? provider "opencode") (string=? provider "opencode-go"))
             "OPENCODE_API_KEY"
            ) ;
            (else #f)
      ) ;cond
    ) ;define

    (define (%format-missing-key-error provider config-dir)
      (let* ((auth-file (path->string (path-join (path config-dir) "auth.json")))
             (env-var (%provider-default-env-var provider))
             (env-hint (if env-var
                         (string-append "set the " env-var " environment variable")
                         "set the corresponding environment variable"
                       ) ;if
             ) ;env-hint
            ) ;
        (string-append "Error: no API key found for provider '" provider
          "'. Configure an 'api_key' entry in " auth-file " or " env-hint "."
        ) ;string-append
      ) ;let*
    ) ;define

    (define (parse-pi-auth-args args)
      (cond
       ((or (null? args)
          (and (pair? args)
            (or (string=? (car args) "-h") (string=? (car args) "--help"))
          ) ;and
        ) ;or
        (make-pi-auth-args :help? #t)
       ) ;
       ((not (or (string=? (car args) "print-api-key") (string=? (car args) "login")))
        (error 'pi-arg-error
          (string-append "unknown auth subcommand '"
            (car args)
            "', currently only 'print-api-key' and 'login' are supported"
          ) ;string-append
        ) ;error
       ) ;
       ((string=? (car args) "login")
        (let loop
          ((rem (cdr args)) (provider #f) (key #f))
          (cond
           ((null? rem)
            (if (or (not provider) (not key))
              (error 'pi-arg-error
                "auth login requires --provider <provider> and --key <key>"
              ) ;error
              (make-pi-auth-args :subcommand "login" :provider provider :key key)
            ) ;if
           ) ;
           ((or (string=? (car rem) "-h") (string=? (car rem) "--help"))
            (make-pi-auth-args :help? #t)
           ) ;
           ((string=? (car rem) "--provider")
            (if (null? (cdr rem))
              (error 'pi-arg-error "auth login requires --provider <provider>")
              (let ((val (cadr rem)))
                (if (or (string-null? (string-trim-both val)) (string-starts? val "-"))
                  (error 'pi-arg-error "auth login requires --provider <provider>")
                  (loop (cddr rem) val key)
                ) ;if
              ) ;let
            ) ;if
           ) ;
           ((string-starts? (car rem) "--provider=")
            (let ((val (substring (car rem) 11 (string-length (car rem)))))
              (if (string-null? (string-trim-both val))
                (error 'pi-arg-error "auth login requires --provider <provider>")
                (loop (cdr rem) val key)
              ) ;if
            ) ;let
           ) ;
           ((string=? (car rem) "--key")
            (if (null? (cdr rem))
              (error 'pi-arg-error "auth login requires --key <key>")
              (let ((val (cadr rem)))
                (if (string-null? (string-trim-both val))
                  (error 'pi-arg-error "auth login requires --key <key>")
                  (loop (cddr rem) provider val)
                ) ;if
              ) ;let
            ) ;if
           ) ;
           ((string-starts? (car rem) "--key=")
            (let ((val (substring (car rem) 6 (string-length (car rem)))))
              (if (string-null? (string-trim-both val))
                (error 'pi-arg-error "auth login requires --key <key>")
                (loop (cdr rem) provider val)
              ) ;if
            ) ;let
           ) ;
           ((string-starts? (car rem) "-")
            (error 'pi-arg-error (string-append "unknown option: " (car rem)))
           ) ;
           (else
             (cond ((not provider) (loop (cdr rem) (car rem) key))
                   ((not key) (loop (cdr rem) provider (car rem)))
                   (else (error 'pi-arg-error (string-append "unexpected extra argument: " (car rem)))
                   ) ;else
             ) ;cond
           ) ;else
          ) ;cond
        ) ;let
       ) ;
       (else
         (let loop
           ((rem (cdr args)) (provider #f))
           (cond ((null? rem)
                  (if (not provider)
                    (error 'pi-arg-error "auth print-api-key requires --provider <provider>")
                    (make-pi-auth-args :subcommand "print-api-key" :provider provider)
                  ) ;if
                 ) ;
                 ((or (string=? (car rem) "-h") (string=? (car rem) "--help"))
                  (make-pi-auth-args :help? #t)
                 ) ;
                 ((string=? (car rem) "--provider")
                  (if (null? (cdr rem))
                    (error 'pi-arg-error "auth print-api-key requires --provider <provider>")
                    (let ((val (cadr rem)))
                      (if (or (string-null? (string-trim-both val)) (string-starts? val "-"))
                        (error 'pi-arg-error "auth print-api-key requires --provider <provider>")
                        (loop (cddr rem) val)
                      ) ;if
                    ) ;let
                  ) ;if
                 ) ;
                 ((string-starts? (car rem) "--provider=")
                  (let ((val (substring (car rem) 11 (string-length (car rem)))))
                    (if (string-null? (string-trim-both val))
                      (error 'pi-arg-error "auth print-api-key requires --provider <provider>")
                      (loop (cdr rem) val)
                    ) ;if
                  ) ;let
                 ) ;
                 ((string-starts? (car rem) "-")
                  (error 'pi-arg-error (string-append "unknown option: " (car rem)))
                 ) ;
                 (else (error 'pi-arg-error (string-append "unexpected extra argument: " (car rem)))
                 ) ;else
           ) ;cond
         ) ;let
       ) ;else
      ) ;cond
    ) ;define

    (define (pi-auth-print-api-key provider . opt-args)
      (let* ((config-dir (if (pair? opt-args) (car opt-args) (gf-get-config-dir)))
             (opt-env
               (if (and (pair? opt-args) (pair? (cdr opt-args))) (list (cadr opt-args)) '())
             ) ;opt-env
             (auth-creds (apply pi-load-auth (cons config-dir opt-env)))
             (key (apply pi-resolve-provider-key (cons provider (cons auth-creds opt-env))))
            ) ;
        (if
          (and (string? key) (not (string-null? (string-trim-both key))))
          (begin
            (display key)
            (newline)
            0
          ) ;begin
          (begin
            (display-stderr (%format-missing-key-error provider config-dir))
            1
          ) ;begin
        ) ;if
      ) ;let*
    ) ;define

    (define (pi-auth-login provider key . opt-args)
      (let* ((config-dir (if (pair? opt-args) (car opt-args) (gf-get-config-dir)))
             (saved (catch #t
                      (lambda () (pi-save-auth-key provider key config-dir))
                      (lambda (tag . info) #f)
                    ) ;catch
             ) ;saved
            ) ;
        (if saved
          (begin
            (display (string-append "Saved API key for provider '" provider "' to " saved "\n")
            ) ;display
            0
          ) ;begin
          (begin
            (display-stderr (string-append "Error: failed to save API key for provider '" provider "'\n")
            ) ;display-stderr
            1
          ) ;begin
        ) ;if
      ) ;let*
    ) ;define

    (define (run-pi-auth args . opt-args)
      (let ((config-dir (if (pair? opt-args) (car opt-args) (gf-get-config-dir)))
            (custom-env
              (if (and (pair? opt-args) (pair? (cdr opt-args))) (cadr opt-args) #f)
            ) ;custom-env
           ) ;
        (catch 'pi-arg-error
          (lambda ()
            (let ((parsed (parse-pi-auth-args args)))
              (cond ((pi-auth-args-help? parsed)
                     (display "Usage: gf pi auth print-api-key --provider <provider>\n       gf pi auth login --provider <provider> --key <key>\n"
                     ) ;display
                     0
                    ) ;
                    ((equal? (pi-auth-args-subcommand parsed) "print-api-key")
                     (let ((provider (pi-auth-args-provider parsed)))
                       (if custom-env
                         (pi-auth-print-api-key provider config-dir custom-env)
                         (pi-auth-print-api-key provider config-dir)
                       ) ;if
                     ) ;let
                    ) ;
                    ((equal? (pi-auth-args-subcommand parsed) "login")
                     (let ((provider (pi-auth-args-provider parsed)) (key (pi-auth-args-key parsed)))
                       (pi-auth-login provider key config-dir)
                     ) ;let
                    ) ;
                    (else 1)
              ) ;cond
            ) ;let
          ) ;lambda
          (lambda (err-type . info)
            (display-stderr (string-append "Error: " (catch-error->string err-type info)))
            1
          ) ;lambda
        ) ;catch
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
