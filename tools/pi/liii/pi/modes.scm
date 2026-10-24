;;
;; Ported from @earendil-works/pi-coding-agent (https://github.com/earendil-works/pi)
;; Original work Copyright (c) 2025 Mario Ortiz Manero / Earendil Works
;; Goldfish Scheme port Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi modes)
  (import (scheme base)
    (scheme process-context)
    (liii string)
    (liii list)
    (liii pi args)
    (liii pi config)
    (liii pi extension types)
    (liii pi extension registry)
    (liii pi modes output)
    (liii pi modes flow)
    (liii pi modes review)
    (liii pi modes simplify)
  ) ;import
  (export display-stderr handle-print-result emit-json-event dispatch-pi
    prepare-system-prompt persist-initial-user-msg! run-agent-loop-with-timeout
    run-session-flow run-compact-flow run-code-review-flow run-simplify-flow
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
             (base-cfg (resolve-pi-config parsed env))
             (cfg (fold (lambda (aug c) (aug c parsed))
                    base-cfg
                    (extension-registry-augmenters reg)
                  ) ;fold
             ) ;cfg
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
                      (if (eq? mode 'json) emit-json-event #f)
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
                    ) ;run-session-flow
                   ) ;
                   ((eq? mode 'json)
                    (run-session-flow parsed cfg initial-msg emit-json-event (lambda _ 0))
                   ) ;
                   (else 0)
             ) ;cond
           ) ;let
         ) ;let
        ) ;if
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
