;;
;; Ported from @earendil-works/pi-coding-agent (https://github.com/earendil-works/pi)
;; Original work Copyright (c) 2025 Mario Ortiz Manero / Earendil Works
;; Goldfish Scheme port Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi agent event)
  (import (scheme base)
    (liii base)
    (liii error)
    (liii json)
    (liii list)
    (liii pi agent event agent-start)
    (liii pi agent event turn-start)
    (liii pi agent event message-start)
    (liii pi agent event message-update)
    (liii pi agent event message-end)
    (liii pi agent event tool-execution-start)
    (liii pi agent event tool-execution-update)
    (liii pi agent event tool-execution-end)
    (liii pi agent event turn-end)
    (liii pi agent event agent-end)
    (liii pi ai message)
    (liii pi ai tools definition)
    (liii pi ai tools result)
    (liii pi ai tools trunc-result)
  ) ;import
  (export pi-agent-event? pi-agent-event-type pi-agent-event->json
    json->pi-agent-event pi-agent-event-normalize-json pi-event-agent-start
    pi-event-agent-start? pi-event-agent-start-type pi-event-turn-start
    pi-event-turn-start? pi-event-turn-start-type pi-event-message-start
    pi-event-message-start? pi-event-message-start-type
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
    pi-event-agent-end-savings
  ) ;export
  (begin

    (define (pi-agent-event? x)
      (or (pi-event-agent-start? x)
        (pi-event-turn-start? x)
        (pi-event-message-start? x)
        (pi-event-message-update? x)
        (pi-event-message-end? x)
        (pi-event-tool-execution-start? x)
        (pi-event-tool-execution-update? x)
        (pi-event-tool-execution-end? x)
        (pi-event-turn-end? x)
        (pi-event-agent-end? x)
      ) ;or
    ) ;define

    (define (pi-agent-event-type ev)
      (cond ((pi-event-agent-start? ev) (pi-event-agent-start-type ev))
            ((pi-event-turn-start? ev) (pi-event-turn-start-type ev))
            ((pi-event-message-start? ev) (pi-event-message-start-type ev))
            ((pi-event-message-update? ev) (pi-event-message-update-type ev))
            ((pi-event-message-end? ev) (pi-event-message-end-type ev))
            ((pi-event-tool-execution-start? ev) (pi-event-tool-execution-start-type ev))
            ((pi-event-tool-execution-update? ev) (pi-event-tool-execution-update-type ev))
            ((pi-event-tool-execution-end? ev) (pi-event-tool-execution-end-type ev))
            ((pi-event-turn-end? ev) (pi-event-turn-end-type ev))
            ((pi-event-agent-end? ev) (pi-event-agent-end-type ev))
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
              (else (cons k (pi-agent-event-normalize-json v)))
        ) ;cond
      ) ;let*
    ) ;define

    (define (pi-agent-event-normalize-json x)
      (cond ((null? x) #())
            ((equal? x '(())) '(()))
            ((pi-agent-event? x) (pi-agent-event->json x))
            ((pi-message? x) (pi-message->json x))
            ((pi-block? x) (pi-block->json x))
            ((pi-tool-result? x) (pi-tool-result->json x))
            ((pi-tool-definition? x)
             (pi-agent-event-normalize-json (list (cons 'name (pi-tool-definition-name x))
                                              (cons 'description (pi-tool-definition-description x))
                                              (cons 'parameters (pi-tool-definition-parameters x))
                                              (cons 'execute (pi-tool-definition-execute x))
                                              (cons 'snippet (pi-tool-definition-snippet x))
                                              (cons 'guidelines (pi-tool-definition-guidelines x))
                                            ) ;list
             ) ;pi-agent-event-normalize-json
            ) ;
            ((pi-trunc-result? x)
             (pi-agent-event-normalize-json (list (cons 'content (pi-trunc-result-content x))
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
             ) ;pi-agent-event-normalize-json
            ) ;
            ((vector? x) (vector-map pi-agent-event-normalize-json x))
            ((and (pair? x) (every %json-object-entry? x))
             (filter (lambda (entry) entry) (map %normalize-json-entry x))
            ) ;
            ((pair? x) (list->vector (map pi-agent-event-normalize-json x)))
            ((symbol? x) (symbol->string x))
            (else x)
      ) ;cond
    ) ;define

    (define (pi-agent-event->json ev)
      (unless (pi-agent-event? ev)
        (type-error "pi-agent-event->json: expected pi-agent-event" ev)
      ) ;unless
      (cond ((pi-event-agent-start? ev) (list (cons "type" "agent_start")))
            ((pi-event-turn-start? ev) (list (cons "type" "turn_start")))
            ((pi-event-message-start? ev)
             (list (cons "type" "message_start")
               (cons "message"
                 (pi-agent-event-normalize-json (pi-event-message-start-message ev))
               ) ;cons
             ) ;list
            ) ;
            ((pi-event-message-update? ev)
             (list (cons "type" "message_update")
               (cons "usage"
                 (pi-agent-event-normalize-json (pi-event-message-update-usage ev))
               ) ;cons
               (cons "assistantMessageEvent"
                 (pi-agent-event-normalize-json (pi-event-message-update-assistant-message-event ev)
                 ) ;pi-agent-event-normalize-json
               ) ;cons
             ) ;list
            ) ;
            ((pi-event-message-end? ev)
             (list (cons "type" "message_end")
               (cons "message"
                 (pi-agent-event-normalize-json (pi-event-message-end-message ev))
               ) ;cons
             ) ;list
            ) ;
            ((pi-event-tool-execution-start? ev)
             (list (cons "type" "tool_execution_start")
               (cons "toolCallId" (pi-event-tool-execution-start-tool-call-id ev))
               (cons "toolName" (pi-event-tool-execution-start-tool-name ev))
               (cons "args"
                 (pi-agent-event-normalize-json (pi-event-tool-execution-start-args ev))
               ) ;cons
             ) ;list
            ) ;
            ((pi-event-tool-execution-update? ev)
             (list (cons "type" "tool_execution_update")
               (cons "toolCallId" (pi-event-tool-execution-update-tool-call-id ev))
               (cons "toolName" (pi-event-tool-execution-update-tool-name ev))
               (cons "args"
                 (pi-agent-event-normalize-json (pi-event-tool-execution-update-args ev))
               ) ;cons
               (cons "partialResult"
                 (pi-agent-event-normalize-json (pi-event-tool-execution-update-partial-result ev)
                 ) ;pi-agent-event-normalize-json
               ) ;cons
             ) ;list
            ) ;
            ((pi-event-tool-execution-end? ev)
             (let ((base
                     (list (cons "type" "tool_execution_end")
                       (cons "toolCallId" (pi-event-tool-execution-end-tool-call-id ev))
                       (cons "toolName" (pi-event-tool-execution-end-tool-name ev))
                       (cons "result"
                         (pi-agent-event-normalize-json (pi-event-tool-execution-end-result ev))
                       ) ;cons
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
               (cons "message" (pi-agent-event-normalize-json (pi-event-turn-end-message ev)))
               (cons "toolResults"
                 (pi-agent-event-normalize-json (pi-event-turn-end-tool-results ev))
               ) ;cons
             ) ;list
            ) ;
            ((pi-event-agent-end? ev)
             (let* ((base
                      (list (cons "type" "agent_end")
                        (cons "messages"
                          (pi-agent-event-normalize-json (pi-event-agent-end-messages ev))
                        ) ;cons
                        (cons "willRetry" (if (pi-event-agent-end-will-retry ev) 'true 'false))
                      ) ;list
                    ) ;base
                    (c (pi-event-agent-end-cost ev))
                    (s (pi-event-agent-end-savings ev))
                    (base
                      (if c (append base (list (cons "cost" (pi-agent-event-normalize-json c)))) base)
                    ) ;base
                    (base
                      (if s
                        (append base (list (cons "savings" (pi-agent-event-normalize-json s))))
                        base
                      ) ;if
                    ) ;base
                   ) ;
               base
             ) ;let*
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

    (define (json->pi-agent-event obj)
      (if (pi-agent-event? obj)
        obj
        (begin
          (unless (or (json-object? obj) (null? obj))
            (type-error "json->pi-agent-event: expected json object or pi-agent-event" obj)
          ) ;unless
          (if (null? obj)
            #f
            (let ((etype (json-ref-string obj "type" "")))
              (cond ((equal? etype "agent_start") (pi-event-agent-start))
                    ((equal? etype "turn_start") (pi-event-turn-start))
                    ((equal? etype "message_start")
                     (pi-event-message-start (%convert-msg (json-ref obj "message")))
                    ) ;
                    ((equal? etype "message_update")
                     (let ((u (json-ref obj "usage")) (ame (json-ref obj "assistantMessageEvent")))
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
                    (else #f)
              ) ;cond
            ) ;let
          ) ;if
        ) ;begin
      ) ;if
    ) ;define

  ) ;begin
) ;define-library
