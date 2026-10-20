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
    (liii pi blueprint args)
    (liii pi compass args)
    (liii pi args files)
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
        (cons 'code-review #f)
        (cons 'auto-review #f)
        (cons 'simplify #f)
        (cons 'red-green #f)
        (cons 'apply #f)
        (cons 'staged #f)
        (cons 'review-ref #f)
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

    (define (handle-flag-or-opt arg next-arg)
      (let-values (((cp-key cp-val cp-consumed) (pi-compass-handle-flag arg next-arg)))
        (if cp-key
          (values cp-key cp-val cp-consumed)
          (let-values (((bp-key bp-val bp-consumed) (pi-blueprint-handle-flag arg next-arg)))
            (if bp-key
              (values bp-key bp-val bp-consumed)
              (cond ((or (string=? arg "-h") (string=? arg "--help")) (values 'help #t 1))
                    ((or (string=? arg "-p") (string=? arg "--print")) (values 'print #t 1))
                    ((or (string=? arg "-c") (string=? arg "--continue")) (values 'continue #t 1))
                    ((string=? arg "--compact") (values 'compact #t 1))
                    ((string=? arg "--no-auto-compact") (values 'no-auto-compact #t 1))
                    ((string=? arg "--no-session") (values 'no-session #t 1))
                    ((string=? arg "--code-review") (values 'code-review #t 1))
                    ((string=? arg "--auto-review") (values 'auto-review #t 1))
                    ((string=? arg "--simplify") (values 'simplify #t 1))
                    ((string=? arg "--red-green") (values 'red-green #t 1))
                    ((string=? arg "--apply") (values 'apply #t 1))
                    ((string=? arg "--staged") (values 'staged #t 1))
                    ((string-starts? arg "--ref=")
                     (values 'review-ref (substring arg 6 (string-length arg)) 1)
                    ) ;
                    ((and (string=? arg "--ref") next-arg) (values 'review-ref next-arg 2))
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
                    (else (values #f #f 0))
              ) ;cond
            ) ;if
          ) ;let-values
        ) ;if
      ) ;let-values
    ) ;define

    (define (apply-flag-or-opt args res)
      (let* ((arg (car args)) (next (if (pair? (cdr args)) (cadr args) #f)))
        (let-values (((key val consumed) (handle-flag-or-opt arg next)))
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

    (define (pi-validate-blueprint-flags parsed)
      (pi-blueprint-validate parsed)
    ) ;define

    (define (pi-validate-review-flags parsed)
      (let ((code-review (pi-args-ref parsed 'code-review))
            (auto-review (pi-args-ref parsed 'auto-review))
            (simplify (pi-args-ref parsed 'simplify))
            (red-green (pi-args-ref parsed 'red-green))
            (apply? (pi-args-ref parsed 'apply))
            (staged (pi-args-ref parsed 'staged))
            (ref (pi-args-ref parsed 'review-ref))
           ) ;
        (when (and code-review auto-review)
          (error 'pi-arg-error "--code-review and --auto-review cannot be used together")
        ) ;when
        (when (and code-review simplify)
          (error 'pi-arg-error "--code-review and --simplify cannot be used together")
        ) ;when
        (when (and red-green (or code-review simplify))
          (error 'pi-arg-error
            "--red-green cannot be used with --code-review or --simplify"
          ) ;error
        ) ;when
        (when (and red-green (or apply? staged ref))
          (error 'pi-arg-error
            "--apply, --staged and --ref are not supported with --red-green"
          ) ;error
        ) ;when
        (when (and apply? (not simplify))
          (error 'pi-arg-error "--apply requires --simplify")
        ) ;when
        (when (and staged (not (or code-review simplify)))
          (error 'pi-arg-error "--staged requires --code-review or --simplify")
        ) ;when
        (when (and ref (not (or code-review simplify)))
          (error 'pi-arg-error "--ref requires --code-review or --simplify")
        ) ;when
        (pi-validate-blueprint-flags parsed)
        (pi-compass-validate parsed)
      ) ;let
    ) ;define

    (define (parse-pi-args argv)
      (let loop
        ((args argv)
         (res (append (default-args-alist)
                (pi-blueprint-args-defaults)
                (pi-compass-args-defaults)
              ) ;append
         ) ;res
         (file-args '())
         (messages '())
        ) ;
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
          (let-values (((next-args next-res) (apply-flag-or-opt args res)))
            (loop next-args next-res file-args messages)
          ) ;let-values
         ) ;
         (else (loop (cdr args) res file-args (cons (car args) messages)))
        ) ;cond
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
