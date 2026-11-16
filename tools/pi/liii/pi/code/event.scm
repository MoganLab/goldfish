;;
;; Ported from @earendil-works/pi-coding-agent (https://github.com/earendil-works/pi)
;; Original work Copyright (c) 2025 Mario Ortiz Manero / Earendil Works
;; Goldfish Scheme port Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code event)
  (import (scheme base)
    (liii base)
    (liii error)
    (liii json)
    (liii uuid)
    (liii pi ai time)
    (liii pi ai message)
    (liii pi agent event)
    (liii pi code event session)
    (liii pi code event agent-settled)
  ) ;import
  (export pi-iso-8601-utc-now pi-current-time-millis pi-serialize-event
    pi-event? pi-event-type pi-event->json json->pi-event pi-event-session
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
    pi-event-tool-execution-end-is-error pi-event-tool-execution-end-duration-ms
    pi-event-turn-end pi-event-turn-end? pi-event-turn-end-type
    pi-event-turn-end-message pi-event-turn-end-tool-results pi-event-agent-end
    pi-event-agent-end? pi-event-agent-end-type pi-event-agent-end-messages
    pi-event-agent-end-will-retry pi-event-agent-end-cost
    pi-event-agent-end-savings pi-event-agent-settled pi-event-agent-settled?
    pi-event-agent-settled-type pi-event-agent-settled-aborted
    pi-build-json-event
  ) ;export
  (begin

    (define (pi-event? x)
      (or (pi-event-session? x) (pi-agent-event? x) (pi-event-agent-settled? x))
    ) ;define

    (define (pi-event-type ev)
      (cond ((pi-event-session? ev) (pi-event-session-type ev))
            ((pi-agent-event? ev) (pi-agent-event-type ev))
            ((pi-event-agent-settled? ev) (pi-event-agent-settled-type ev))
            (else #f)
      ) ;cond
    ) ;define

    (define (pi-event->json ev)
      (unless (pi-event? ev)
        (type-error "pi-event->json: expected pi-event" ev)
      ) ;unless
      (cond
       ((pi-event-session? ev)
        (list (cons "type" "session")
          (cons "version" (pi-event-session-version ev))
          (cons "id" (pi-event-session-id ev))
          (cons "timestamp" (pi-event-session-timestamp ev))
          (cons "cwd" (pi-event-session-cwd ev))
        ) ;list
       ) ;
       ((pi-agent-event? ev) (pi-agent-event->json ev))
       ((pi-event-agent-settled? ev)
        (list (cons "type" "agent_settled")
          (cons "aborted" (if (pi-event-agent-settled-aborted ev) 'true 'false))
        ) ;list
       ) ;
      ) ;cond
    ) ;define

    (define (%decode-session obj)
      (let ((ver (json-ref obj "version"))
            (id (json-ref obj "id"))
            (ts (json-ref obj "timestamp"))
            (cwd (json-ref-string obj "cwd" ""))
           ) ;
        (pi-event-session cwd
          (if (and (string? id) (> (string-length id) 0)) id (uuid7))
          (if (and (string? ts) (> (string-length ts) 0)) ts (pi-iso-8601-utc-now))
          (if (number? ver) ver 3)
        ) ;pi-event-session
      ) ;let
    ) ;define

    (define (json->pi-event obj)
      (if (pi-event? obj)
        obj
        (begin
          (unless (or (json-object? obj) (null? obj))
            (type-error "json->pi-event: expected json object or pi-event" obj)
          ) ;unless
          (if (null? obj)
            #f
            (let ((etype (json-ref-string obj "type" "")))
              (cond ((equal? etype "session") (%decode-session obj))
                    ((equal? etype "agent_settled")
                     (pi-event-agent-settled (json-ref-boolean obj "aborted" #f))
                    ) ;
                    (else (json->pi-agent-event obj))
              ) ;cond
            ) ;let
          ) ;if
        ) ;begin
      ) ;if
    ) ;define

    (define (pi-build-json-event ev-type args)
      (case ev-type
       ((session) (apply pi-event-session args))
       ((agent_start) (pi-event-agent-start))
       ((turn_start) (pi-event-turn-start))
       ((message_start) (pi-event-message-start (car args)))
       ((message_update) (pi-event-message-update (car args) (cadr args)))
       ((message_end) (pi-event-message-end (car args)))
       ((tool_execution_start) (apply pi-event-tool-execution-start args))
       ((tool_execution_update) (apply pi-event-tool-execution-update args))
       ((tool_execution_end) (apply pi-event-tool-execution-end args))
       ((turn_end) (pi-event-turn-end (car args) (cadr args)))
       ((agent_end) (apply pi-event-agent-end args))
       ((agent_settled) (pi-event-agent-settled (car args)))
       (else #f)
      ) ;case
    ) ;define

    (define (pi-serialize-event obj)
      (json->string (cond ((pi-event? obj) (pi-event->json obj))
                          ((pi-message? obj) (pi-message->json obj))
                          ((pi-block? obj) (pi-block->json obj))
                          (else (pi-agent-event-normalize-json obj))
                    ) ;cond
      ) ;json->string
    ) ;define

  ) ;begin
) ;define-library
