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
    (liii pi ai message)
    (liii pi ai time)
    (liii pi ai dispatch)
    (liii pi agent tools)
    (liii pi agent cost)
    (liii pi agent budget)
  ) ;import
  (export pi-step-agent-turn pi-notify-turn-end pi-emit-agent-end
    pi-finish-agent-turn pi-emit-agent-error pi-maybe-compact pi-handle-tool-use
    run-agent-loop pi-resolve-turn-system-prompt
  ) ;export
  (begin

    ;; 从 config 取扩展事件桥接（由上层注入；缺失时不向扩展系统转发事件）
    (define (%ext-emit config)
      (let ((pair (and (pair? config) (assoc 'emit-extension-event config))))
        (and pair (procedure? (cdr pair)) (cdr pair))
      ) ;let
    ) ;define

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

    (define* (pi-resolve-turn-system-prompt system-prompt
               tool-calls
               (turn #f)
               (max-turns #f)
               (remaining-sec #f)
               (deadline-sec #f)
               (session-timeout #f)
             ) ;pi-resolve-turn-system-prompt
      (let ((base (if (procedure? system-prompt) (system-prompt tool-calls) system-prompt))
           ) ;
        (if (number? turn)
          (let ((budget-status (pi-format-turn-budget-status turn max-turns
                                 remaining-sec deadline-sec session-timeout
                               ) ;pi-format-turn-budget-status
                ) ;budget-status
               ) ;
            (string-append base "\n\n" budget-status)
          ) ;let
          base
        ) ;if
      ) ;let
    ) ;define*

    (define* (run-agent-loop config
               initial-messages
               system-prompt
               (emit #f)
               (timeout 600)
               (max-turns 200)
               (session-timeout 600)
               (compaction #f)
               (request-fn pi-request-provider-stream)
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
             (ext-emit (%ext-emit config))
            ) ;
        (let loop
          ((msgs initial-messages) (turn 0) (last-tool-calls '()))
          (cond ((and (number? session-timeout) (< session-timeout 0))
                 (pi-emit-agent-error (%make-session-timeout-error) msgs emit config)
                ) ;
                ((and deadline-sec (> (pi-current-monotonic-seconds) deadline-sec))
                 (pi-emit-agent-error (%make-session-timeout-error) msgs emit config)
                ) ;
                ((and (> max-turns 0) (>= turn max-turns))
                 (pi-emit-agent-error (%make-max-turns-error) msgs emit config)
                ) ;
                (else
                  (when ext-emit
                    (ext-emit 'turn_start (list (cons 'turn turn)))
                  ) ;when
                  (let* ((msgs (pi-maybe-compact compaction msgs))
                         (now (pi-current-monotonic-seconds))
                         (rem-sec (if deadline-sec (- deadline-sec now) #f))
                         (turn-sp (pi-resolve-turn-system-prompt system-prompt
                                    last-tool-calls turn max-turns rem-sec
                                    deadline-sec session-timeout
                                  ) ;pi-resolve-turn-system-prompt
                         ) ;turn-sp
                        ) ;
                    (let*-values (((new-msgs stop-reason tool-calls resp)
                                   (pi-step-agent-turn config msgs turn-sp emit timeout request-fn)
                                  ) ;
                                 ) ;
                      (if (equal? stop-reason "toolUse")
                        (let*-values (((all-msgs terminated?)
                                       (pi-handle-tool-use new-msgs tool-calls emit timeout config resp)
                                      ) ;
                                     ) ;
                          (if terminated?
                            (pi-finish-agent-turn all-msgs "stop" '() emit config resp)
                            (begin
                              (when (procedure? emit)
                                (emit 'turn_start)
                              ) ;when
                              (loop all-msgs (+ turn 1) tool-calls)
                            ) ;begin
                          ) ;if
                        ) ;let*-values
                        (pi-finish-agent-turn new-msgs stop-reason tool-calls emit config resp)
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
