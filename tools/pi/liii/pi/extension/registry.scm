;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi extension registry)
  (import (scheme base)
    (liii error)
    (liii pi extension types)
    (liii pi tools record)
  ) ;import
  (export target->registry register-tool! extension-register-tool!
    find-extension-tool extension-registry-tools-list register-flag!
    extension-register-flag! find-extension-flag extension-registry-flags-list
    register-command! extension-register-command! find-extension-command
    extension-registry-commands-list extension-registry-get-listeners
    current-extension-registry set-current-extension-registry!
    with-extension-registry
  ) ;export
  (begin

    (define (target->registry target)
      (cond ((extension-registry? target) target)
            ((extension-api? target) (extension-api-registry target))
            ((extension-context? target) (extension-context-registry target))
            (else (type-error "target must be an extension-api, extension-context, or extension-registry"
                  ) ;type-error
            ) ;else
      ) ;cond
    ) ;define

    ;; ----------------- Tool -----------------

    (define (normalize-tool-def def)
      (cond ((extension-tool-def? def) def)
            ((pi-tool-definition? def)
             (make-extension-tool-def (pi-tool-definition-name def)
               (pi-tool-definition-description def)
               (pi-tool-definition-parameters def)
               (pi-tool-definition-execute def)
               :snippet    (pi-tool-definition-snippet def)
               :guidelines (pi-tool-definition-guidelines def)
             ) ;make-extension-tool-def
            ) ;
            ((pair? def)
             (let ((name (cond ((assq 'name def) => cdr) (else #f)))
                   (desc (cond ((assq 'description def) => cdr) (else #f)))
                   (params (cond ((assq 'parameters def) => cdr) (else '())))
                   (exec (cond ((assq 'execute def) => cdr) (else #f)))
                   (snip (cond ((assq 'snippet def) => cdr) (else "")))
                   (guides (cond ((assq 'guidelines def) => cdr) (else '())))
                  ) ;
               (make-extension-tool-def name desc params exec :snippet snip :guidelines guides)
             ) ;let
            ) ;
            (else (type-error "tool-def must be an extension-tool-def or alist"))
      ) ;cond
    ) ;define

    (define (upsert-by-key lst key-fn new-item)
      (let loop
        ((rest lst) (acc '()) (replaced #f))
        (if (null? rest)
          (if replaced (reverse acc) (reverse (cons new-item acc)))
          (let ((item (car rest)))
            (if (string=? (key-fn item) (key-fn new-item))
              (loop (cdr rest) (cons new-item acc) #t)
              (loop (cdr rest) (cons item acc) replaced)
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (register-tool! target tool-def)
      (let* ((reg (target->registry target))
             (tool (normalize-tool-def tool-def))
             (tools (extension-registry-tools reg))
             (updated (upsert-by-key tools extension-tool-def-name tool))
            ) ;
        (set-extension-registry-tools! reg updated)
        tool
      ) ;let*
    ) ;define

    (define extension-register-tool! register-tool!)

    (define (find-extension-tool target name)
      (let* ((reg (target->registry target)) (tools (extension-registry-tools reg)))
        (let loop
          ((rest tools))
          (cond ((null? rest) #f)
                ((string=? (extension-tool-def-name (car rest)) name) (car rest))
                (else (loop (cdr rest)))
          ) ;cond
        ) ;let
      ) ;let*
    ) ;define

    (define (extension-registry-tools-list target)
      (extension-registry-tools (target->registry target))
    ) ;define

    ;; ----------------- Flag -----------------

    (define (normalize-flag-def def)
      (cond ((extension-flag-def? def) def)
            ((pair? def)
             (let ((flag (cond ((assq 'flag def) => cdr) (else #f)))
                   (desc (cond ((assq 'description def) => cdr) (else #f)))
                   (handler (cond ((assq 'handler def) => cdr) (else #f)))
                   (type (cond ((assq 'type def) => cdr) (else 'boolean)))
                   (default (cond ((assq 'default def) => cdr) (else #f)))
                  ) ;
               (make-extension-flag-def flag desc :handler handler :type type :default default)
             ) ;let
            ) ;
            (else (type-error "flag-def must be an extension-flag-def or alist"))
      ) ;cond
    ) ;define

    (define (register-flag! target flag-def)
      (let* ((reg (target->registry target))
             (flag (normalize-flag-def flag-def))
             (flags (extension-registry-flags reg))
             (updated (upsert-by-key flags extension-flag-def-flag flag))
            ) ;
        (set-extension-registry-flags! reg updated)
        flag
      ) ;let*
    ) ;define

    (define extension-register-flag! register-flag!)

    (define (find-extension-flag target flag-name)
      (let* ((reg (target->registry target)) (flags (extension-registry-flags reg)))
        (let loop
          ((rest flags))
          (cond ((null? rest) #f)
                ((string=? (extension-flag-def-flag (car rest)) flag-name) (car rest))
                (else (loop (cdr rest)))
          ) ;cond
        ) ;let
      ) ;let*
    ) ;define

    (define (extension-registry-flags-list target)
      (extension-registry-flags (target->registry target))
    ) ;define

    ;; ----------------- Command -----------------

    (define (normalize-command-def def)
      (cond ((extension-command-def? def) def)
            ((pair? def)
             (let ((name (cond ((assq 'name def) => cdr) (else #f)))
                   (desc (cond ((assq 'description def) => cdr) (else #f)))
                   (handler (cond ((assq 'handler def) => cdr) (else #f)))
                   (params (cond ((assq 'params def) => cdr) (else '())))
                  ) ;
               (make-extension-command-def name desc handler :params params)
             ) ;let
            ) ;
            (else (type-error "command-def must be an extension-command-def or alist"))
      ) ;cond
    ) ;define

    (define (register-command! target command-def)
      (let* ((reg (target->registry target))
             (cmd (normalize-command-def command-def))
             (cmds (extension-registry-commands reg))
             (updated (upsert-by-key cmds extension-command-def-name cmd))
            ) ;
        (set-extension-registry-commands! reg updated)
        cmd
      ) ;let*
    ) ;define

    (define extension-register-command! register-command!)

    (define (find-extension-command target name)
      (let* ((reg (target->registry target)) (cmds (extension-registry-commands reg)))
        (let loop
          ((rest cmds))
          (cond ((null? rest) #f)
                ((string=? (extension-command-def-name (car rest)) name) (car rest))
                (else (loop (cdr rest)))
          ) ;cond
        ) ;let
      ) ;let*
    ) ;define

    (define (extension-registry-commands-list target)
      (extension-registry-commands (target->registry target))
    ) ;define

    ;; ----------------- Listeners -----------------

    (define (extension-registry-get-listeners target event-type)
      (let* ((reg (target->registry target))
             (evt-sym (cond ((symbol? event-type) event-type)
                            ((string? event-type) (string->symbol event-type))
                            (else (type-error "event-type must be a symbol or string"))
                      ) ;cond
             ) ;evt-sym
             (entry (assq evt-sym (extension-registry-listeners reg)))
            ) ;
        (if entry (cdr entry) '())
      ) ;let*
    ) ;define

    ;; ----------------- Active Registry -----------------

    (define *current-extension-registry* #f)

    (define (current-extension-registry)
      *current-extension-registry*
    ) ;define

    (define (set-current-extension-registry! target)
      (if (not target)
        (begin
          (set! *current-extension-registry* #f)
          #f
        ) ;begin
        (let ((reg (target->registry target)))
          (set! *current-extension-registry* reg)
          reg
        ) ;let
      ) ;if
    ) ;define

    (define (with-extension-registry target thunk)
      (let ((prev *current-extension-registry*)
            (reg (if (not target) #f (target->registry target)))
           ) ;
        (dynamic-wind (lambda () (set! *current-extension-registry* reg))
          thunk
          (lambda () (set! *current-extension-registry* prev))
        ) ;dynamic-wind
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
