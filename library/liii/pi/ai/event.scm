;;
;; Ported from @earendil-works/pi-coding-agent (https://github.com/earendil-works/pi)
;; Original work Copyright (c) 2025 Mario Ortiz Manero / Earendil Works
;; Goldfish Scheme port Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai event)
  (import (scheme base)
    (liii base)
    (liii error)
    (liii json)
    (liii list)
    (liii uuid)
    (liii pi ai time)
    (liii pi ai event session)
    (liii pi ai event agent-start)
    (liii pi ai event turn-start)
    (liii pi ai event message-start)
    (liii pi ai event message-update)
    (liii pi ai event message-end)
    (liii pi ai event tool-execution-start)
    (liii pi ai event tool-execution-update)
    (liii pi ai event tool-execution-end)
    (liii pi ai event turn-end)
    (liii pi ai event agent-end)
    (liii pi ai event agent-settled)
    (liii pi ai message)
    (liii pi ai tools definition)
    (liii pi ai tools result)
    (liii pi ai tools trunc-result)
  ) ;import
  (export pi-iso-8601-utc-now pi-current-time-millis pi-serialize-event
    pi-event? pi-event-type pi-event->json json->pi-event
    pi-event-session pi-event-session? pi-event-session-type
    pi-event-session-version pi-event-session-id pi-event-session-timestamp
    pi-event-session-cwd pi-event-agent-start pi-event-agent-start?
    pi-event-agent-start-type pi-event-turn-start pi-event-turn-start?
    pi-event-turn-start-type pi-event-message-start pi-event-message-start?
    pi-event-message-start-type pi-event-message-start-message
    pi-event-message-update pi-event-message-update?
    pi-event-message-update-type pi-event-message-update-usage
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

    (define (%json-object-entry? x)
      (and (pair? x) (not (pair? (car x))) (or (symbol? (car x)) (string? (car x))))
    ) ;define
    (define (%normalize-json-entry entry)
      (let* ((raw-k (car entry))
             (k (if (symbol? raw-k) (symbol->string raw-k) raw-k))
             (v (cdr entry))
            ) ;
        (cond ((and (string=? k "errorMessage") (not v)) #f)
              ((and (or (string=? k "arguments") (string=? k "input"))
                 (or (null? v) (equal? v '(())) (not v) (eq? v 'null))
               ) ;and
               (cons k '(()))
              ) ;
              (else (cons k (%normalize-json-value v)))
        ) ;cond
      ) ;let*
    ) ;define
    (define (%normalize-json-value x)
      (cond ((null? x) #())
            ((equal? x '(())) '(()))
            ((pi-event? x) (pi-event->json x))
            ((pi-message? x) (pi-message->json x))
            ((pi-block? x) (pi-block->json x))
            ((pi-tool-result? x) (pi-tool-result->json x))
            ((pi-tool-definition? x)
             (%normalize-json-value (list (cons 'name (pi-tool-definition-name x))
                                      (cons 'description (pi-tool-definition-description x))
                                      (cons 'parameters (pi-tool-definition-parameters x))
                                      (cons 'execute (pi-tool-definition-execute x))
                                      (cons 'snippet (pi-tool-definition-snippet x))
                                      (cons 'guidelines (pi-tool-definition-guidelines x))
                                    ) ;list
             ) ;%normalize-json-value
            ) ;
            ((pi-trunc-result? x)
             (%normalize-json-value (list (cons 'content (pi-trunc-result-content x))
                                      (cons 'truncated (pi-trunc-result-truncated x))
                                      (cons 'truncated-by (pi-trunc-result-truncated-by x))
                                      (cons 'total-lines (pi-trunc-result-total-lines x))
                                      (cons 'total-bytes (pi-trunc-result-total-bytes x))
                                      (cons 'output-lines (pi-trunc-result-output-lines x))
                                      (cons 'output-bytes (pi-trunc-result-output-bytes x))
                                      (cons 'first-line-exceeds-limit? (pi-trunc-result-first-line-exceeds-limit? x))
                                      (cons 'first-line-truncated (pi-trunc-result-first-line-exceeds-limit? x))
                                      (cons 'last-line-partial? (pi-trunc-result-last-line-partial? x))
                                      (cons 'last-line-truncated (pi-trunc-result-last-line-partial? x))
                                    ) ;list
             ) ;%normalize-json-value
            ) ;
            ((vector? x) (vector-map %normalize-json-value x))
            ((and (pair? x) (every %json-object-entry? x))
             (filter (lambda (entry) entry) (map %normalize-json-entry x))
            ) ;
            ((pair? x) (list->vector (map %normalize-json-value x)))
            ((symbol? x) (symbol->string x))
            (else x)
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
       ((pi-event-agent-start? ev) (list (cons "type" "agent_start")))
       ((pi-event-turn-start? ev) (list (cons "type" "turn_start")))
       ((pi-event-message-start? ev)
        (list (cons "type" "message_start")
          (cons "message" (%normalize-json-value (pi-event-message-start-message ev)))
        ) ;list
       ) ;
       ((pi-event-message-update? ev)
        (list (cons "type" "message_update")
          (cons "usage" (%normalize-json-value (pi-event-message-update-usage ev)))
          (cons "assistantMessageEvent"
            (%normalize-json-value (pi-event-message-update-assistant-message-event ev))
          ) ;cons
        ) ;list
       ) ;
       ((pi-event-message-end? ev)
        (list (cons "type" "message_end")
          (cons "message" (%normalize-json-value (pi-event-message-end-message ev)))
        ) ;list
       ) ;
       ((pi-event-tool-execution-start? ev)
        (list (cons "type" "tool_execution_start")
          (cons "toolCallId" (pi-event-tool-execution-start-tool-call-id ev))
          (cons "toolName" (pi-event-tool-execution-start-tool-name ev))
          (cons "args" (%normalize-json-value (pi-event-tool-execution-start-args ev)))
        ) ;list
       ) ;
       ((pi-event-tool-execution-update? ev)
        (list (cons "type" "tool_execution_update")
          (cons "toolCallId" (pi-event-tool-execution-update-tool-call-id ev))
          (cons "toolName" (pi-event-tool-execution-update-tool-name ev))
          (cons "args" (%normalize-json-value (pi-event-tool-execution-update-args ev)))
          (cons "partialResult"
            (%normalize-json-value (pi-event-tool-execution-update-partial-result ev))
          ) ;cons
        ) ;list
       ) ;
       ((pi-event-tool-execution-end? ev)
        (let ((base (list (cons "type" "tool_execution_end")
                      (cons "toolCallId" (pi-event-tool-execution-end-tool-call-id ev))
                      (cons "toolName" (pi-event-tool-execution-end-tool-name ev))
                      (cons "result" (%normalize-json-value (pi-event-tool-execution-end-result ev)))
                      (cons "isError" (if (pi-event-tool-execution-end-is-error ev) 'true 'false))
                    ) ;list
              ) ;base
              (duration (pi-event-tool-execution-end-duration-ms ev))
             ) ;
          (if (number? duration) (append base (list (cons "durationMs" duration))) base)
        ) ;let
       ) ;
       ((pi-event-turn-end? ev)
        (list (cons "type" "turn_end")
          (cons "message" (%normalize-json-value (pi-event-turn-end-message ev)))
          (cons "toolResults" (%normalize-json-value (pi-event-turn-end-tool-results ev)))
        ) ;list
       ) ;
       ((pi-event-agent-end? ev)
        (let* ((base (list (cons "type" "agent_end")
                       (cons "messages" (%normalize-json-value (pi-event-agent-end-messages ev)))
                       (cons "willRetry" (if (pi-event-agent-end-will-retry ev) 'true 'false))
                     ) ;list
               ) ;base
               (c (pi-event-agent-end-cost ev))
               (s (pi-event-agent-end-savings ev))
               (base
                 (if c (append base (list (cons "cost" (%normalize-json-value c)))) base)
               ) ;base
               (base
                 (if s (append base (list (cons "savings" (%normalize-json-value s)))) base)
               ) ;base
              ) ;
          base
        ) ;let*
       ) ;
       ((pi-event-agent-settled? ev)
        (list (cons "type" "agent_settled")
          (cons "aborted" (if (pi-event-agent-settled-aborted ev) 'true 'false))
        ) ;list
       ) ;
      ) ;cond
    ) ;define

    (define (%convert-msg x)
      (cond ((or (null? x) (json-null? x)) #f)
            ((json-object? x) (or (json->pi-message x) x))
            (else x)
      ) ;cond
    ) ;define
    (define (%convert-result x)
      (cond ((or (null? x) (json-null? x)) #f)
            ((pi-tool-result? x) x)
            ((and (json-object? x) (equal? (json-ref x "role") "toolResult"))
             (or (json->pi-message x) x)
            ) ;
            ((json-object? x) (json->pi-tool-result x))
            (else x)
      ) ;cond
    ) ;define
    (define (%convert-list fn xs)
      (cond ((vector? xs) (map fn (vector->list xs)))
            ((list? xs) (map fn xs))
            (else '())
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
    (define (%decode-tool-execution-end obj)
      (let ((dur (json-ref obj "durationMs")))
        (pi-event-tool-execution-end (json-ref-string obj "toolCallId" "")
          (json-ref-string obj "toolName" "")
          (%convert-result (json-ref obj "result"))
          (json-ref-boolean obj "isError" #f)
          (if (number? dur) dur #f)
        ) ;pi-event-tool-execution-end
      ) ;let
    ) ;define
    (define (%decode-agent-end obj)
      (let ((c (json-ref obj "cost")) (s (json-ref obj "savings")))
        (pi-event-agent-end (%convert-list %convert-msg (json-ref obj "messages"))
          (json-ref-boolean obj "willRetry" #f)
          (if (or (null? c) (json-null? c)) #f c)
          (if (or (null? s) (json-null? s)) #f s)
        ) ;pi-event-agent-end
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
                    ((equal? etype "agent_start") (pi-event-agent-start))
                    ((equal? etype "turn_start") (pi-event-turn-start))
                    ((equal? etype "message_start")
                     (pi-event-message-start (%convert-msg (json-ref obj "message")))
                    ) ;
                    ((equal? etype "message_update")
                     (let ((u (json-ref obj "usage"))
                           (ame (json-ref obj "assistantMessageEvent"))
                          ) ;
                       (pi-event-message-update (if (or (null? u) (json-null? u)) #f u)
                         (if (or (null? ame) (json-null? ame)) #f ame)
                       ) ;pi-event-message-update
                     ) ;let
                    ) ;
                    ((equal? etype "message_end")
                     (pi-event-message-end (%convert-msg (json-ref obj "message")))
                    ) ;
                    ((equal? etype "tool_execution_start")
                     (let ((args (json-ref obj "args")))
                       (pi-event-tool-execution-start (json-ref-string obj "toolCallId" "")
                         (json-ref-string obj "toolName" "")
                         (if (or (null? args) (json-null? args)) '(()) args)
                       ) ;pi-event-tool-execution-start
                     ) ;let
                    ) ;
                    ((equal? etype "tool_execution_update")
                     (let ((args (json-ref obj "args")))
                       (pi-event-tool-execution-update (json-ref-string obj "toolCallId" "")
                         (json-ref-string obj "toolName" "")
                         (if (or (null? args) (json-null? args)) '(()) args)
                         (%convert-result (json-ref obj "partialResult"))
                       ) ;pi-event-tool-execution-update
                     ) ;let
                    ) ;
                    ((equal? etype "tool_execution_end") (%decode-tool-execution-end obj))
                    ((equal? etype "turn_end")
                     (pi-event-turn-end (%convert-msg (json-ref obj "message"))
                       (%convert-list %convert-result (json-ref obj "toolResults"))
                     ) ;pi-event-turn-end
                    ) ;
                    ((equal? etype "agent_end") (%decode-agent-end obj))
                    ((equal? etype "agent_settled")
                     (pi-event-agent-settled (json-ref-boolean obj "aborted" #f))
                    ) ;
                    (else #f)
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
      (json->string
        (cond ((pi-event? obj) (pi-event->json obj))
              ((pi-message? obj) (pi-message->json obj))
              ((pi-block? obj) (pi-block->json obj))
              (else (%normalize-json-value obj))
        ) ;cond
      ) ;json->string
    ) ;define
  ) ;begin
) ;define-library
