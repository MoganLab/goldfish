;;
;; Ported from @earendil-works/pi-coding-agent (https://github.com/earendil-works/pi)
;; Original work Copyright (c) 2025 Mario Ortiz Manero / Earendil Works
;; Goldfish Scheme port Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi agent loop)
  (import (scheme base)
    (liii string)
    (liii list)
    (liii pi message)
    (liii pi time)
    (liii pi agent tools)
    (liii pi agent cost)
    (liii pi agent budget)
    (liii pi extension registry)
    (liii pi extension event)
  ) ;import
  (export pi-step-agent-turn pi-notify-turn-end pi-emit-agent-end
    pi-finish-agent-turn pi-emit-agent-error pi-maybe-compact
    pi-get-active-registry pi-handle-tool-use
  ) ;export
  (begin

    (define (pi-step-agent-turn config msgs system-prompt emit timeout request-fn)
      (let* ((active-tools (cdr (assoc 'tools config)))
             (provider (cdr (or (assoc 'provider config) '(#f . "google"))))
             (on-stream-ev
               (if (procedure? emit)
                 (lambda (ev)
                   (let ((usage (pi-agent-assoc-val 'usage "usage" ev '())))
                     (emit 'message_update usage ev)
                   ) ;let
                 ) ;lambda
                 #f
               ) ;if
             ) ;on-stream-ev
             (raw-resp (request-fn provider config msgs system-prompt
                         active-tools on-stream-ev timeout
                       ) ;request-fn
             ) ;raw-resp
             (resp (pi-agent-attach-cost raw-resp config))
            ) ;
        (when (procedure? emit)
          (emit 'message_start resp)
          (emit 'message_end resp)
        ) ;when
        (let* ((new-msgs (append msgs (list resp)))
               (stop-reason (pi-agent-assoc-val 'stopReason "stopReason" resp ""))
               (content (pi-agent-assoc-val 'content "content" resp '()))
               (tool-calls (pi-filter-tool-calls content))
              ) ;
          (values new-msgs stop-reason tool-calls resp)
        ) ;let*
      ) ;let*
    ) ;define

    (define (pi-notify-turn-end emit assistant-msg tool-results)
      (when (procedure? emit)
        (emit 'turn_end assistant-msg tool-results)
      ) ;when
    ) ;define

    (define (pi-emit-agent-end emit final-msgs stop-reason config)
      (when (procedure? emit)
        (let* ((aborted? (equal? stop-reason "aborted"))
               (summary (pi-agent-session-cost-summary final-msgs config))
               (cost (cdr (assoc 'totalCost summary)))
               (savings (cdr (assoc 'totalSavings summary)))
              ) ;
          (emit 'agent_end final-msgs #f cost savings)
          (emit 'agent_settled aborted?)
        ) ;let*
      ) ;when
    ) ;define

    (define (pi-finish-agent-turn new-msgs stop-reason tool-calls emit config resp)
      (let ((final-msgs
              (if (equal? stop-reason "length")
                (if (null? tool-calls)
                  new-msgs
                  (append new-msgs (pi-fail-tool-calls-length tool-calls emit))
                ) ;if
                new-msgs
              ) ;if
            ) ;final-msgs
           ) ;
        (pi-notify-turn-end emit resp #())
        (pi-emit-agent-end emit final-msgs stop-reason config)
        final-msgs
      ) ;let
    ) ;define

    (define (pi-emit-agent-error err-msg msgs emit config)
      (let* ((resp (pi-agent-attach-cost err-msg config))
             (final-msgs (append msgs (list resp)))
            ) ;
        (when (procedure? emit)
          (emit 'turn_start)
          (emit 'message_start resp)
          (emit 'message_end resp)
          (emit 'turn_end resp #())
          (pi-emit-agent-end emit final-msgs "error" config)
        ) ;when
        final-msgs
      ) ;let*
    ) ;define

    (define (pi-maybe-compact compaction msgs)
      (if
        (and (pair? compaction)
          (procedure? (cdr (or (assoc 'compactor compaction) '(#f . #f))))
        ) ;and
        (let ((res ((cdr (assoc 'compactor compaction)) msgs)))
          (if (and (pair? res) (list? res)) res msgs)
        ) ;let
        msgs
      ) ;if
    ) ;define

    (define (pi-get-active-registry config)
      (cond ((and (pair? config) (assoc 'registry config)) => cdr)
            ((current-extension-registry))
            (else (default-extension-registry))
      ) ;cond
    ) ;define

    (define (pi-handle-tool-use new-msgs tool-calls emit timeout config resp)
      (let*-values (((tools-res terminated?)
                     (pi-execute-tool-calls-with-status tool-calls emit timeout config)
                    ) ;
                   ) ;
        (let* ((all-msgs (append new-msgs tools-res))
               (res-vec (pi-extract-tool-result-blocks all-msgs (length tools-res)))
              ) ;
          (pi-notify-turn-end emit resp res-vec)
          (values all-msgs terminated?)
        ) ;let*
      ) ;let*-values
    ) ;define

  ) ;begin
) ;define-library
