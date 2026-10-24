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
    (liii pi ai gemini)
    (liii pi ai anthropic)
    (liii pi ai kimi)
    (liii pi ai deepseek)
    (liii pi ai minimax)
    (liii pi ai zai)
    (liii pi ai message)
    (liii pi ai time)
    (liii pi agent tools)
    (liii pi agent budget)
    (liii pi agent loop)
    (liii pi extension event)
  ) ;import
  (export run-agent-loop extract-assistant-text pi-request-provider-stream
    pi-resolve-turn-system-prompt
  ) ;export
  (begin

    (define (%collect-text-blocks blks)
      (map (lambda (blk) (pi-agent-assoc-val 'text "text" blk ""))
        (filter (lambda (blk) (equal? (pi-agent-assoc-val 'type "type" blk "") "text"))
          blks
        ) ;filter
      ) ;map
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
                 (pi-emit-agent-error (%make-session-timeout-error) msgs emit config)
                ) ;
                ((and deadline-sec (> (pi-current-monotonic-seconds) deadline-sec))
                 (pi-emit-agent-error (%make-session-timeout-error) msgs emit config)
                ) ;
                ((and (> max-turns 0) (>= turn max-turns))
                 (pi-emit-agent-error (%make-max-turns-error) msgs emit config)
                ) ;
                (else (pi-emit! (pi-get-active-registry config) 'turn_start (list (cons 'turn turn)))
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
                                   (pi-step-agent-turn config msgs turn-sp emit timeout pi-request-provider-stream)
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
