;;
;; Ported from @earendil-works/pi-coding-agent (https://github.com/earendil-works/pi)
;; Original work Copyright (c) 2025 Mario Ortiz Manero / Earendil Works
;; Goldfish Scheme port Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code modes)
  (import (scheme base)
    (scheme process-context)
    (liii string)
    (liii list)
    (liii pi code cli args)
    (liii pi code config)
    (liii pi code core extensions types)
    (liii pi code core extensions registry)
    (liii pi code modes output)
    (liii pi code modes flow)
    (liii pi code modes verbose)
  ) ;import
  (export display-stderr handle-print-result emit-json-event dispatch-pi
    prepare-system-prompt persist-initial-user-msg! run-agent-loop-with-timeout
    run-session-flow run-compact-flow resolve-app-mode make-verbose-emitter
    handle-verbose-result display-verbose-event display-agent-event
  ) ;export
  (begin

    (define (%find-matching-flow flows parsed local-only?)
      (find
        (lambda (flow)
          (and (if local-only?
                 (extension-flow-def-local? flow)
                 (not (extension-flow-def-local? flow))
               ) ;if
           ((extension-flow-def-predicate flow) parsed)
          ) ;and
        ) ;lambda
        flows
      ) ;find
    ) ;define

    (define (dispatch-pi parsed mode initial-msg rest-msgs)
      (let* ((reg (or (current-extension-registry) (default-extension-registry)))
             (env (get-environment-variables))
             (cfg (augment-pi-config (resolve-pi-config parsed env) parsed))
             (flows (extension-registry-flows reg))
             (matched-local (%find-matching-flow flows parsed #t))
            ) ;
        (if matched-local
         ((extension-flow-def-handler matched-local) parsed cfg mode initial-msg)
         (let ((cred-msg (pi-missing-credential-msg cfg)))
           (when cred-msg
             (error 'credential-error cred-msg)
           ) ;when
           (let ((matched-custom (%find-matching-flow flows parsed #f)))
             (cond (matched-custom ((extension-flow-def-handler matched-custom) parsed cfg mode initial-msg)
                   ) ;matched-custom
                   ((pi-args-ref parsed 'compact)
                    (run-compact-flow parsed
                      cfg
                      initial-msg
                      (cond ((eq? mode 'json) emit-json-event)
                            ((eq? mode 'verbose) (make-verbose-emitter))
                            (else #f)
                      ) ;cond
                    ) ;run-compact-flow
                   ) ;
                   ((eq? mode 'print)
                    (run-session-flow parsed
                      cfg
                      initial-msg
                      #f
                      (lambda (res-msgs)
                        (handle-print-result (and (pair? res-msgs) (car (reverse res-msgs))))
                      ) ;lambda
                      #f
                      rest-msgs
                    ) ;run-session-flow
                   ) ;
                   ((eq? mode 'verbose)
                    (run-session-flow parsed
                      cfg
                      initial-msg
                      (make-verbose-emitter)
                      (lambda (res-msgs)
                        (handle-verbose-result (and (pair? res-msgs) (car (reverse res-msgs))))
                      ) ;lambda
                      #f
                      rest-msgs
                    ) ;run-session-flow
                   ) ;
                   ((eq? mode 'json)
                    (run-session-flow parsed
                      cfg
                      initial-msg
                      emit-json-event
                      (lambda _ 0)
                      #f
                      rest-msgs
                    ) ;run-session-flow
                   ) ;
                   (else 0)
             ) ;cond
           ) ;let
         ) ;let
        ) ;if
      ) ;let*
    ) ;define

    (define (resolve-app-mode mode-option print-flag stdin-tty stdout-tty)
      (cond ((or (equal? mode-option "json") (eq? mode-option 'json)) 'json)
            ((or (equal? mode-option "verbose") (eq? mode-option 'verbose)) 'verbose)
            ((or (equal? mode-option "text") (eq? mode-option 'text)) 'print)
            ((or print-flag (not stdin-tty) (not stdout-tty)) 'verbose)
            (else 'interactive)
      ) ;cond
    ) ;define

  ) ;begin
) ;define-library
