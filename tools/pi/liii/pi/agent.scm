;;
;; Ported from @earendil-works/pi-coding-agent (https://github.com/earendil-works/pi)
;; Original work Copyright (c) 2025 Mario Ortiz Manero / Earendil Works
;; Goldfish Scheme port Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi agent)
  (import (scheme base)
    (liii string)
    (liii list)
    (liii pi gemini)
    (liii pi anthropic)
    (liii pi kimi)
    (liii pi deepseek)
    (liii pi minimax)
    (liii pi zai)
    (liii pi message)
    (liii pi time)
    (liii pi event)
    (liii pi agent tools)
    (liii pi agent cost)
    (liii pi extension registry)
    (liii pi extension event)
  ) ;import
  (export run-agent-loop extract-assistant-text pi-request-provider-stream
    pi-resolve-turn-system-prompt
  ) ;export
  (begin

    (define (%collect-text-blocks blks)
      (let loop
        ((rest blks) (acc '()))
        (if (null? rest)
          (reverse acc)
          (let* ((blk (car rest)) (t-val (pi-agent-assoc-val 'type "type" blk "")))
            (if (equal? t-val "text")
              (let ((t (pi-agent-assoc-val 'text "text" blk "")))
                (loop (cdr rest) (cons t acc))
              ) ;let
              (loop (cdr rest) acc)
            ) ;if
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    (define (extract-assistant-text msg)
      (let* ((content (pi-agent-assoc-val 'content "content" msg '()))
             (texts (%collect-text-blocks content))
            ) ;
        (string-join texts "\n")
      ) ;let*
    ) ;define

    (define (%make-max-turns-error)
      (pi-assistant-message '()
        :stop-reason   "error"
        :error-message "Maximum turns limit reached"
        :usage         '((input . 0) (output . 0) (total . 0))
      ) ;pi-assistant-message
    ) ;define

    (define (%make-session-timeout-error)
      (pi-assistant-message '()
        :stop-reason "error"
        :error-message
        "Session timeout exceeded: total execution deadline reached"
        :usage '((input . 0) (output . 0) (total . 0))
      ) ;pi-assistant-message
    ) ;define

    (define (pi-resolve-turn-system-prompt system-prompt tool-calls)
      (if (procedure? system-prompt) (system-prompt tool-calls) system-prompt)
    ) ;define

    (define (pi-request-provider-stream provider config msgs sys-prompt tools on-ev timeout)
      (cond ((or (equal? provider "anthropic") (equal? provider "glm"))
             (anthropic-request-stream config msgs sys-prompt tools on-ev timeout)
            ) ;
            ((equal? provider "deepseek")
             (deepseek-request-stream config msgs sys-prompt tools on-ev timeout)
            ) ;
            ((equal? provider "kimi-coding")
             (kimi-request-stream config msgs sys-prompt tools on-ev timeout)
            ) ;
            ((or (equal? provider "zai-coding-cn") (equal? provider "zai"))
             (zai-request-stream config msgs sys-prompt tools on-ev timeout)
            ) ;
            ((or (equal? provider "minimax") (equal? provider "minimax-cn"))
             (minimax-request-stream config msgs sys-prompt tools on-ev timeout)
            ) ;
            (else (gemini-request-stream config msgs sys-prompt tools on-ev timeout))
      ) ;cond
    ) ;define

    (define (%step-agent-turn config msgs system-prompt emit timeout)
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
             (raw-resp (pi-request-provider-stream provider config msgs
                         system-prompt active-tools on-stream-ev timeout
                       ) ;pi-request-provider-stream
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

    (define (%notify-turn-end emit assistant-msg tool-results)
      (when (procedure? emit)
        (emit 'turn_end assistant-msg tool-results)
      ) ;when
    ) ;define

    (define (%emit-agent-end emit final-msgs stop-reason config)
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

    (define (%finish-agent-turn new-msgs stop-reason tool-calls emit config resp)
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
        (%notify-turn-end emit resp #())
        (%emit-agent-end emit final-msgs stop-reason config)
        final-msgs
      ) ;let
    ) ;define

    (define (%emit-agent-error err-msg msgs emit config)
      (let* ((resp (pi-agent-attach-cost err-msg config))
             (final-msgs (append msgs (list resp)))
             (summary (pi-agent-session-cost-summary final-msgs config))
             (cost (cdr (assoc 'totalCost summary)))
             (savings (cdr (assoc 'totalSavings summary)))
            ) ;
        (when (procedure? emit)
          (emit 'turn_start)
          (emit 'message_start resp)
          (emit 'message_end resp)
          (emit 'turn_end resp #())
          (emit 'agent_end final-msgs #f cost savings)
          (emit 'agent_settled #f)
        ) ;when
        final-msgs
      ) ;let*
    ) ;define

    (define (%maybe-compact compaction msgs)
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

    (define (%get-active-registry config)
      (or (and (pair? config) (assoc 'registry config) (cdr (assoc 'registry config)))
        (current-extension-registry)
        (default-extension-registry)
      ) ;or
    ) ;define

    (define (%handle-tool-use new-msgs tool-calls emit timeout config resp)
      (let*-values (((tools-res terminated?)
                     (pi-execute-tool-calls-with-status tool-calls emit timeout config)
                    ) ;
                   ) ;
        (let* ((all-msgs (append new-msgs tools-res))
               (res-vec (pi-extract-tool-result-blocks all-msgs (length tools-res)))
              ) ;
          (%notify-turn-end emit resp res-vec)
          (values all-msgs terminated?)
        ) ;let*
      ) ;let*-values
    ) ;define

    (define* (run-agent-loop config
               initial-messages
               system-prompt
               (emit #f)
               (timeout 600)
               (max-turns 200)
               (session-timeout 600)
               (compaction #f)
             ) ;run-agent-loop
      (when (procedure? emit)
        (emit 'agent_start)
        (emit 'turn_start)
        (for-each (lambda (m) (emit 'message_start m) (emit 'message_end m))
          initial-messages
        ) ;for-each
      ) ;when
      (let* ((start-sec (pi-current-monotonic-seconds))
             (deadline-sec (if (and (number? session-timeout) (> session-timeout 0))
                             (+ start-sec session-timeout)
                             #f
                           ) ;if
             ) ;deadline-sec
            ) ;
        (let loop
          ((msgs initial-messages) (turn 0) (last-tool-calls '()))
          (cond ((and (number? session-timeout) (< session-timeout 0))
                 (%emit-agent-error (%make-session-timeout-error) msgs emit config)
                ) ;
                ((and deadline-sec (> (pi-current-monotonic-seconds) deadline-sec))
                 (%emit-agent-error (%make-session-timeout-error) msgs emit config)
                ) ;
                ((and (> max-turns 0) (>= turn max-turns))
                 (%emit-agent-error (%make-max-turns-error) msgs emit config)
                ) ;
                (else (pi-emit! (%get-active-registry config) 'turn_start (list (cons 'turn turn)))
                  (let* ((msgs (%maybe-compact compaction msgs))
                         (turn-sp (pi-resolve-turn-system-prompt system-prompt last-tool-calls))
                        ) ;
                    (let*-values (((new-msgs stop-reason tool-calls resp)
                                   (%step-agent-turn config msgs turn-sp emit timeout)
                                  ) ;
                                 ) ;
                      (if (equal? stop-reason "toolUse")
                        (let*-values (((all-msgs terminated?)
                                       (%handle-tool-use new-msgs tool-calls emit timeout config resp)
                                      ) ;
                                     ) ;
                          (if terminated?
                            (%finish-agent-turn all-msgs "stop" '() emit config resp)
                            (begin
                              (when (procedure? emit)
                                (emit 'turn_start)
                              ) ;when
                              (loop all-msgs (+ turn 1) tool-calls)
                            ) ;begin
                          ) ;if
                        ) ;let*-values
                        (%finish-agent-turn new-msgs stop-reason tool-calls emit config resp)
                      ) ;if
                    ) ;let*-values
                  ) ;let*
                ) ;else
          ) ;cond
        ) ;let
      ) ;let*
    ) ;define*

  ) ;begin
) ;define-library
