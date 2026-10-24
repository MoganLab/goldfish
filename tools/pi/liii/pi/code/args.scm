;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code args)
  (import (scheme base)
    (scheme process-context)
    (liii string)
    (liii path)
    (liii pi code args files)
    (liii pi code args query)
    (liii pi code extension types)
    (liii pi code extension registry)
    (liii pi code extension discovery)
    (liii pi code extension loader)
  ) ;import
  (export parse-pi-args pi-args-ref pi-resolve-timeout format-file-block
    process-file-arguments pi-validate-review-flags
  ) ;export
  (begin

    (define *builtin-extensions-loaded* #f)

    (define (ensure-builtin-extensions! reg)
      (unless *builtin-extensions-loaded*
        (set! *builtin-extensions-loaded* #t)
        (let ((paths (pi-scan-extension-dir (pi-default-builtin-extensions-dir))))
          (pi-load-extensions paths reg)
        ) ;let
      ) ;unless
      reg
    ) ;define

    (add-registry-initializer! ensure-builtin-extensions!)

    (define (default-args-alist)
      (list (cons 'help #f)
        (cons 'print #f)
        (cons 'mode #f)
        (cons 'model #f)
        (cons 'provider #f)
        (cons 'thinking-level #f)
        (cons 'proxy #f)
        (cons 'tools #f)
        (cons 'exclude-tools #f)
        (cons 'no-tools #f)
        (cons 'max-turns 200)
        (cons 'timeout #f)
        (cons 'session-timeout #f)
        (cons 'no-session #f)
        (cons 'continue #f)
        (cons 'compact #f)
        (cons 'no-auto-compact #f)
        (cons 'list-models #f)
        (cons 'system-prompt #f)
        (cons 'append-system-prompt '())
        (cons 'file-args '())
        (cons 'messages '())
        (cons 'unknown-flags '())
      ) ;list
    ) ;define

    (define (parse-positionals rest file-args messages)
      (if (null? rest)
        (values (reverse file-args) (reverse messages))
        (let ((arg (car rest)))
          (if (string-starts? arg "@")
            (parse-positionals (cdr rest)
              (cons (substring arg 1 (string-length arg)) file-args)
              messages
            ) ;parse-positionals
            (parse-positionals (cdr rest) file-args (cons arg messages))
          ) ;if
        ) ;let
      ) ;if
    ) ;define

    (define (%match-prefix-flag arg prefix)
      (let ((full-prefix (string-append prefix "=")))
        (if (string-starts? arg full-prefix)
          (substring arg (string-length full-prefix) (string-length arg))
          #f
        ) ;if
      ) ;let
    ) ;define

    (define (%match-extension-flag reg arg next-arg)
      (if (not reg)
        (values #f #f 0)
        (let loop
          ((flags (extension-registry-flags reg)))
          (if (null? flags)
            (values #f #f 0)
            (let* ((f (car flags))
                   (flag-name (extension-flag-def-flag f))
                   (aliases (extension-flag-def-aliases f))
                   (key (extension-flag-def-key f))
                   (takes-arg? (extension-flag-def-takes-arg? f))
                   (handler (extension-flag-def-handler f))
                   (matches? (or (string=? arg flag-name) (member arg aliases)))
                  ) ;
              (cond
               ((and matches? (procedure? handler))
                (let-values (((v consumed) (handler arg next-arg)))
                  (values key v consumed)
                ) ;let-values
               ) ;
               ((and takes-arg? (%match-prefix-flag arg flag-name))
                =>
                (lambda (val) (values key val 1))
               ) ;
               ((and takes-arg?
                  (let check-als
                    ((als aliases))
                    (cond ((null? als) #f)
                          ((%match-prefix-flag arg (car als)) => (lambda (val) val))
                          (else (check-als (cdr als)))
                    ) ;cond
                  ) ;let
                ) ;and
                =>
                (lambda (val) (values key val 1))
               ) ;
               ((and takes-arg? matches?)
                (if next-arg (values key next-arg 2) (values key #t 1))
               ) ;
               (matches? (values key #t 1))
               (else (loop (cdr flags)))
              ) ;cond
            ) ;let*
          ) ;if
        ) ;let
      ) ;if
    ) ;define

    (define (handle-flag-or-opt arg next-arg reg)
      (cond ((or (string=? arg "-h") (string=? arg "--help")) (values 'help #t 1))
            ((or (string=? arg "-p") (string=? arg "--print")) (values 'print #t 1))
            ((or (string=? arg "-c") (string=? arg "--continue")) (values 'continue #t 1))
            ((string=? arg "--compact") (values 'compact #t 1))
            ((string=? arg "--no-auto-compact") (values 'no-auto-compact #t 1))
            ((string=? arg "--no-session") (values 'no-session #t 1))
            ((or (string=? arg "--no-tools") (string=? arg "--no-builtin-tools"))
             (values 'no-tools #t 1)
            ) ;
            ((string=? arg "--list-models")
             (if (and next-arg
                   (not (string-starts? next-arg "-"))
                   (not (string-starts? next-arg "@"))
                 ) ;and
               (values 'list-models next-arg 2)
               (values 'list-models #t 1)
             ) ;if
            ) ;
            ((and (string=? arg "--mode") next-arg) (values 'mode next-arg 2))
            ((and (string=? arg "--model") next-arg) (values 'model next-arg 2))
            ((and (string=? arg "--provider") next-arg) (values 'provider next-arg 2))
            ((and (string=? arg "--thinking") next-arg)
             (if (member next-arg '("off" "minimal" "low" "medium" "high"
                                    "xhigh" "max"))
               (values 'thinking-level next-arg 2)
               (error 'pi-arg-error
                 (string-append "Invalid thinking level: \""
                   next-arg
                   "\". Allowed values: off, minimal, low, medium, high, xhigh, max"
                 ) ;string-append
               ) ;error
             ) ;if
            ) ;
            ((and (string=? arg "--proxy") next-arg) (values 'proxy next-arg 2))
            ((and (string=? arg "--tools") next-arg) (values 'tools next-arg 2))
            ((and (string=? arg "--exclude-tools") next-arg)
             (values 'exclude-tools next-arg 2)
            ) ;
            ((and (string=? arg "--max-turns") next-arg)
             (values 'max-turns (string->number next-arg) 2)
            ) ;
            ((and (string=? arg "--timeout") next-arg)
             (values 'timeout (string->number next-arg) 2)
            ) ;
            ((and (string=? arg "--session-timeout") next-arg)
             (values 'session-timeout (string->number next-arg) 2)
            ) ;
            ((and (string=? arg "--system-prompt") next-arg)
             (values 'system-prompt next-arg 2)
            ) ;
            ((and (string=? arg "--append-system-prompt") next-arg)
             (values 'append-system-prompt next-arg 2)
            ) ;
            (else (%match-extension-flag reg arg next-arg))
      ) ;cond
    ) ;define

    (define (apply-flag-or-opt args res reg)
      (let* ((arg (car args)) (next (if (pair? (cdr args)) (cadr args) #f)))
        (let-values (((key val consumed) (handle-flag-or-opt arg next reg)))
          (cond
           ((eq? key 'append-system-prompt)
            (let ((cur (pi-args-ref res 'append-system-prompt '())))
              (values (cddr args)
                (pi-args-set res 'append-system-prompt (append cur (list val)))
              ) ;values
            ) ;let
           ) ;
           ((and key (= consumed 1)) (values (cdr args) (pi-args-set res key val)))
           ((and key (= consumed 2)) (values (cddr args) (pi-args-set res key val)))
           (else
             (let ((cur-unknown (pi-args-ref res 'unknown-flags '())))
               (values (cdr args) (pi-args-set res 'unknown-flags (cons arg cur-unknown)))
             ) ;let
           ) ;else
          ) ;cond
        ) ;let-values
      ) ;let*
    ) ;define

    (define (pi-validate-review-flags parsed)
      (let* ((reg (or (current-extension-registry) (default-extension-registry)))
             (_ (ensure-builtin-extensions! reg))
             (validators (extension-registry-validators reg))
            ) ;
        (for-each (lambda (v) (v parsed)) validators)
        #t
      ) ;let*
    ) ;define

    (define (parse-pi-args argv)
      (let* ((reg (or (current-extension-registry) (default-extension-registry)))
             (_ (ensure-builtin-extensions! reg))
             (ext-flags (extension-registry-flags reg))
             (ext-defaults
               (map (lambda (f) (cons (extension-flag-def-key f) (extension-flag-def-default f)))
                 ext-flags
               ) ;map
             ) ;ext-defaults
             (initial-res (append (default-args-alist) ext-defaults))
            ) ;
        (let loop
          ((args argv) (res initial-res) (file-args '()) (messages '()))
          (cond
           ((null? args)
            (pi-args-set (pi-args-set res 'file-args (reverse file-args))
              'messages
              (reverse messages)
            ) ;pi-args-set
           ) ;
           ((string=? (car args) "--")
            (let-values (((f-args m-args) (parse-positionals (cdr args) file-args messages)))
              (pi-args-set (pi-args-set res 'file-args f-args) 'messages m-args)
            ) ;let-values
           ) ;
           ((string-starts? (car args) "@")
            (let ((arg (car args)))
              (loop (cdr args)
                res
                (cons (substring arg 1 (string-length arg)) file-args)
                messages
              ) ;loop
            ) ;let
           ) ;
           ((string-starts? (car args) "-")
            (let-values (((next-args next-res) (apply-flag-or-opt args res reg)))
              (loop next-args next-res file-args messages)
            ) ;let-values
           ) ;
           (else (loop (cdr args) res file-args (cons (car args) messages)))
          ) ;cond
        ) ;let
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
