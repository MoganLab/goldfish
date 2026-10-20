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
    (liii pi args)
    (liii pi config)
    (liii pi extension types)
    (liii pi extension registry)
    (liii pi extension builtin flows)
    (liii pi modes output)
    (liii pi modes flow)
    (liii pi modes review)
    (liii pi modes simplify)
    (liii pi modes red-green)
  ) ;import
  (export display-stderr handle-print-result emit-json-event dispatch-pi
    prepare-system-prompt persist-initial-user-msg! run-agent-loop-with-timeout
    run-session-flow run-compact-flow run-code-review-flow run-simplify-flow
    run-red-green-flow pi-tdd-extend-tool-names pi-auto-review-system-prompt
  ) ;export
  (begin

    (define (%find-matching-flow flows parsed local-only?)
      (let loop
        ((rest flows))
        (cond ((null? rest) #f)
              ((and
                 (if local-only?
                   (extension-flow-def-local? (car rest))
                   (not (extension-flow-def-local? (car rest)))
                 ) ;if
                 ((extension-flow-def-predicate (car rest)) parsed)
               ) ;and
               (car rest)
              ) ;
              (else (loop (cdr rest)))
        ) ;cond
      ) ;let
    ) ;define

    (define (dispatch-pi parsed mode initial-msg rest-msgs)
      (let* ((reg (or (current-extension-registry) (default-extension-registry)))
             (env (get-environment-variables))
             (base-cfg (resolve-pi-config parsed env))
             (cfg
               (let apply-aug
                 ((augs (extension-registry-augmenters reg)) (c base-cfg))
                 (if (null? augs) c (apply-aug (cdr augs) ((car augs) c parsed)))
               ) ;let
             ) ;cfg
             (flows (extension-registry-flows reg))
             (matched-local (%find-matching-flow flows parsed #t))
            ) ;
        (if matched-local
         ((extension-flow-def-handler matched-local) parsed cfg mode initial-msg)
         (let* ((provider (cdr (or (assoc 'provider cfg) '(#f . "google"))))
                (api-key (cdr (or (assoc 'api-key cfg) '(#f . #f))))
               ) ;
           (when (or (not api-key) (and (string? api-key) (string-null? api-key)))
             (error 'credential-error
               (string-append "No credentials found for provider \"" provider "\"")
             ) ;error
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
         ) ;let*
        ) ;if
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
