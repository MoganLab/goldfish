;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai event record)
  (import (scheme base)
    (liii base)
    (liii uuid)
    (liii pi ai time)
    (liii pi ai event types)
  ) ;import
  (export pi-event? pi-event-type pi-event->alist pi-event-session
    pi-event-session? pi-event-session-type pi-event-session-version
    pi-event-session-id pi-event-session-timestamp pi-event-session-cwd
    pi-event-agent-start pi-event-agent-start? pi-event-agent-start-type
    pi-event-turn-start pi-event-turn-start? pi-event-turn-start-type
    pi-event-message-start pi-event-message-start? pi-event-message-start-type
    pi-event-message-start-message pi-event-message-update
    pi-event-message-update? pi-event-message-update-type
    pi-event-message-update-usage
    pi-event-message-update-assistant-message-event pi-event-message-end
    pi-event-message-end? pi-event-message-end-type pi-event-message-end-message
    pi-event-tool-execution-start pi-event-tool-execution-start?
    pi-event-tool-execution-start-type
    pi-event-tool-execution-start-tool-call-id
    pi-event-tool-execution-start-tool-name pi-event-tool-execution-start-args
    pi-event-tool-execution-update pi-event-tool-execution-update?
    pi-event-tool-execution-update-type
    pi-event-tool-execution-update-tool-call-id
    pi-event-tool-execution-update-tool-name pi-event-tool-execution-update-args
    pi-event-tool-execution-update-partial-result pi-event-tool-execution-end
    pi-event-tool-execution-end? pi-event-tool-execution-end-type
    pi-event-tool-execution-end-tool-call-id
    pi-event-tool-execution-end-tool-name pi-event-tool-execution-end-result
    pi-event-tool-execution-end-is-error pi-event-turn-end pi-event-turn-end?
    pi-event-turn-end-type pi-event-turn-end-message
    pi-event-turn-end-tool-results pi-event-agent-end pi-event-agent-end?
    pi-event-agent-end-type pi-event-agent-end-messages
    pi-event-agent-end-will-retry pi-event-agent-end-cost
    pi-event-agent-end-savings pi-event-agent-settled pi-event-agent-settled?
    pi-event-agent-settled-type pi-event-agent-settled-aborted
  ) ;export
  (begin

    (define (pi-event-session-type ev)
      "session"
    ) ;define
    (define (pi-event-agent-start-type ev)
      "agent_start"
    ) ;define
    (define (pi-event-turn-start-type ev)
      "turn_start"
    ) ;define
    (define (pi-event-message-start-type ev)
      "message_start"
    ) ;define
    (define (pi-event-message-update-type ev)
      "message_update"
    ) ;define
    (define (pi-event-message-end-type ev)
      "message_end"
    ) ;define
    (define (pi-event-tool-execution-start-type ev)
      "tool_execution_start"
    ) ;define
    (define (pi-event-tool-execution-update-type ev)
      "tool_execution_update"
    ) ;define
    (define (pi-event-tool-execution-end-type ev)
      "tool_execution_end"
    ) ;define
    (define (pi-event-turn-end-type ev)
      "turn_end"
    ) ;define
    (define (pi-event-agent-end-type ev)
      "agent_end"
    ) ;define
    (define (pi-event-agent-settled-type ev)
      "agent_settled"
    ) ;define

    (define (pi-event? x)
      (or (pi-event-session? x)
        (pi-event-agent-start? x)
        (pi-event-turn-start? x)
        (pi-event-message-start? x)
        (pi-event-message-update? x)
        (pi-event-message-end? x)
        (pi-event-tool-execution-start? x)
        (pi-event-tool-execution-update? x)
        (pi-event-tool-execution-end? x)
        (pi-event-turn-end? x)
        (pi-event-agent-end? x)
        (pi-event-agent-settled? x)
      ) ;or
    ) ;define

    (define (pi-event-type ev)
      (cond ((pi-event-session? ev) (pi-event-session-type ev))
            ((pi-event-agent-start? ev) (pi-event-agent-start-type ev))
            ((pi-event-turn-start? ev) (pi-event-turn-start-type ev))
            ((pi-event-message-start? ev) (pi-event-message-start-type ev))
            ((pi-event-message-update? ev) (pi-event-message-update-type ev))
            ((pi-event-message-end? ev) (pi-event-message-end-type ev))
            ((pi-event-tool-execution-start? ev) (pi-event-tool-execution-start-type ev))
            ((pi-event-tool-execution-update? ev) (pi-event-tool-execution-update-type ev))
            ((pi-event-tool-execution-end? ev) (pi-event-tool-execution-end-type ev))
            ((pi-event-turn-end? ev) (pi-event-turn-end-type ev))
            ((pi-event-agent-end? ev) (pi-event-agent-end-type ev))
            ((pi-event-agent-settled? ev) (pi-event-agent-settled-type ev))
            (else #f)
      ) ;cond
    ) ;define

    (define (pi-event->alist ev)
      (cond
       ((pi-event-session? ev)
        (list (cons "type" "session")
          (cons "version" (pi-event-session-version ev))
          (cons "id" (pi-event-session-id ev))
          (cons "timestamp" (pi-event-session-timestamp ev))
          (cons "cwd" (pi-event-session-cwd ev))
        ) ;list
       ) ;
       ((pi-event-agent-start? ev) (list (cons "type" "agent_start")))
       ((pi-event-turn-start? ev) (list (cons "type" "turn_start")))
       ((pi-event-message-start? ev)
        (list (cons "type" "message_start")
          (cons "message" (pi-event-message-start-message ev))
        ) ;list
       ) ;
       ((pi-event-message-update? ev)
        (list (cons "type" "message_update")
          (cons "usage" (pi-event-message-update-usage ev))
          (cons "assistantMessageEvent"
            (pi-event-message-update-assistant-message-event ev)
          ) ;cons
        ) ;list
       ) ;
       ((pi-event-message-end? ev)
        (list (cons "type" "message_end")
          (cons "message" (pi-event-message-end-message ev))
        ) ;list
       ) ;
       ((pi-event-tool-execution-start? ev)
        (list (cons "type" "tool_execution_start")
          (cons "toolCallId" (pi-event-tool-execution-start-tool-call-id ev))
          (cons "toolName" (pi-event-tool-execution-start-tool-name ev))
          (cons "args" (pi-event-tool-execution-start-args ev))
        ) ;list
       ) ;
       ((pi-event-tool-execution-update? ev)
        (list (cons "type" "tool_execution_update")
          (cons "toolCallId" (pi-event-tool-execution-update-tool-call-id ev))
          (cons "toolName" (pi-event-tool-execution-update-tool-name ev))
          (cons "args" (pi-event-tool-execution-update-args ev))
          (cons "partialResult" (pi-event-tool-execution-update-partial-result ev))
        ) ;list
       ) ;
       ((pi-event-tool-execution-end? ev)
        (list (cons "type" "tool_execution_end")
          (cons "toolCallId" (pi-event-tool-execution-end-tool-call-id ev))
          (cons "toolName" (pi-event-tool-execution-end-tool-name ev))
          (cons "result" (pi-event-tool-execution-end-result ev))
          (cons "isError" (pi-event-tool-execution-end-is-error ev))
        ) ;list
       ) ;
       ((pi-event-turn-end? ev)
        (list (cons "type" "turn_end")
          (cons "message" (pi-event-turn-end-message ev))
          (cons "toolResults" (pi-event-turn-end-tool-results ev))
        ) ;list
       ) ;
       ((pi-event-agent-end? ev)
        (let* ((base (list (cons "type" "agent_end")
                       (cons "messages" (pi-event-agent-end-messages ev))
                       (cons "willRetry" (pi-event-agent-end-will-retry ev))
                     ) ;list
               ) ;base
               (c (pi-event-agent-end-cost ev))
               (s (pi-event-agent-end-savings ev))
               (base
                 (if c (append base (list (cons "cost" c))) base)
               ) ;base
               (base
                 (if s (append base (list (cons "savings" s))) base)
               ) ;base
              ) ;
          base
        ) ;let*
       ) ;
       ((pi-event-agent-settled? ev)
        (list (cons "type" "agent_settled")
          (cons "aborted" (pi-event-agent-settled-aborted ev))
        ) ;list
       ) ;
       (else #f)
      ) ;cond
    ) ;define

    (define* (pi-event-session cwd (id #f) (timestamp #f))
      (%make-pi-event-session 3
        (or id (uuid7))
        (or timestamp (pi-iso-8601-utc-now))
        cwd
      ) ;%make-pi-event-session
    ) ;define*

    (define (pi-event-agent-start)
      (%make-pi-event-agent-start)
    ) ;define

    (define (pi-event-turn-start)
      (%make-pi-event-turn-start)
    ) ;define

    (define (pi-event-message-start msg)
      (%make-pi-event-message-start msg)
    ) ;define

    (define (pi-event-message-update usage assistant-message-event)
      (%make-pi-event-message-update usage assistant-message-event)
    ) ;define

    (define (pi-event-message-end msg)
      (%make-pi-event-message-end msg)
    ) ;define

    (define (pi-event-tool-execution-start tool-call-id tool-name args)
      (%make-pi-event-tool-execution-start tool-call-id tool-name args)
    ) ;define

    (define (pi-event-tool-execution-update tool-call-id tool-name args partial-result)
      (%make-pi-event-tool-execution-update tool-call-id tool-name args
        partial-result
      ) ;%make-pi-event-tool-execution-update
    ) ;define

    (define (pi-event-tool-execution-end tool-call-id tool-name result is-error)
      (%make-pi-event-tool-execution-end tool-call-id
        tool-name
        result
        (if is-error #t #f)
      ) ;%make-pi-event-tool-execution-end
    ) ;define

    (define* (pi-event-turn-end assistant-msg (tool-results #()))
      (%make-pi-event-turn-end assistant-msg tool-results)
    ) ;define*

    (define* (pi-event-agent-end messages (will-retry #f) (cost #f) (savings #f))
      (%make-pi-event-agent-end messages (if will-retry #t #f) cost savings)
    ) ;define*

    (define* (pi-event-agent-settled (aborted #f))
      (%make-pi-event-agent-settled (if aborted #t #f))
    ) ;define*

  ) ;begin
) ;define-library
