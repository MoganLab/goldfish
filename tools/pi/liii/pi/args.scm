;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi args)
  (import (scheme base)
    (scheme process-context)
    (liii string)
    (liii path)
    (liii pi args files)
    (liii pi extension types)
    (liii pi extension registry)
    (liii pi extension builtin flags)
  ) ;import
  (export parse-pi-args pi-args-ref format-file-block process-file-arguments
    pi-validate-review-flags
  ) ;export
  (begin

    (define (pi-args-ref parsed key . default-val)
      (let ((pair (assoc key parsed)))
        (if pair (cdr pair) (if (pair? default-val) (car default-val) #f))
      ) ;let
    ) ;define

    (define (pi-args-set parsed key val)
      (cons (cons key val)
        (let loop
          ((rest parsed))
          (cond ((null? rest) '())
                ((eq? (caar rest) key) (cdr rest))
                (else (cons (car rest) (loop (cdr rest))))
          ) ;cond
        ) ;let
      ) ;cons
    ) ;define

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
        (cons 'timeout 120)
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
            ((and (or (string=? arg "--thinking-level") (string=? arg "--thinking"))
               next-arg
             ) ;and
             (if (member next-arg '("off" "low" "medium" "high"))
               (values 'thinking-level next-arg 2)
               (error 'pi-arg-error
                 (string-append "Invalid thinking level: \""
                   next-arg
                   "\". Allowed values: off, low, medium, high"
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
             (validators (extension-registry-validators reg))
            ) ;
        (for-each (lambda (v) (v parsed)) validators)
        #t
      ) ;let*
    ) ;define

    (define (parse-pi-args argv)
      (let* ((reg (or (current-extension-registry) (default-extension-registry)))
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
