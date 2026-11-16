;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai event json)
  (import (scheme base)
    (liii json)
    (liii list)
    (liii pi ai event types)
    (liii pi ai event record)
    (liii pi ai message)
    (liii pi ai tools result)
  ) ;import
  (export json->pi-event)
  (begin

    (define (%json-dict-pair obj str-key sym-key)
      (let loop
        ((rest obj))
        (cond ((not (pair? rest)) #f)
              ((and (pair? (car rest))
                 (let ((k (car (car rest))))
                   (or (equal? k str-key) (equal? k sym-key))
                 ) ;let
               ) ;and
               (car rest)
              ) ;
              (else (loop (cdr rest)))
        ) ;cond
      ) ;let
    ) ;define

    (define (%json-dict-ref obj key default-val)
      (if (not (pair? obj))
        default-val
        (let* ((str-key (if (symbol? key) (symbol->string key) key))
               (sym-key (if (string? key) (string->symbol key) key))
               (pair (%json-dict-pair obj str-key sym-key))
              ) ;
          (if pair (let ((v (cdr pair))) (if (eq? v 'null) default-val v)) default-val)
        ) ;let*
      ) ;if
    ) ;define

    (define (%normalize-json-bool val)
      (case val
       ((true) #t)
       ((false) #f)
       (else (if val #t #f))
      ) ;case
    ) ;define

    (define (%json-key->string k)
      (if (symbol? k) (symbol->string k) k)
    ) ;define

    (define (%convert-msg x)
      (if (json-object? x) (or (json->pi-message x) x) x)
    ) ;define

    (define (%convert-result x)
      (cond ((pi-tool-result? x) x)
            ((and (pair? x)
               (equal? (%json-key->string (%json-dict-ref x "role" #f)) "toolResult")
             ) ;and
             (if (json-object? x) (or (json->pi-message x) x) x)
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
      (let ((ver (%json-dict-ref obj "version" #f))
            (id (%json-dict-ref obj "id" #f))
            (ts (%json-dict-ref obj "timestamp" #f))
            (cwd (%json-dict-ref obj "cwd" ""))
           ) ;
        (if (and (number? ver) id ts)
          (%make-pi-event-session ver id ts cwd)
          (pi-event-session cwd id ts)
        ) ;if
      ) ;let
    ) ;define

    (define (%decode-tool-execution-end obj)
      (let ((dur (%json-dict-ref obj "durationMs" #f)))
        (pi-event-tool-execution-end (%json-dict-ref obj "toolCallId" "")
          (%json-dict-ref obj "toolName" "")
          (%convert-result (%json-dict-ref obj "result" #f))
          (%normalize-json-bool (%json-dict-ref obj "isError" #f))
          (if (number? dur) dur #f)
        ) ;pi-event-tool-execution-end
      ) ;let
    ) ;define

    (define (%decode-agent-end obj)
      (pi-event-agent-end (%convert-list %convert-msg (%json-dict-ref obj "messages" '()))
        (%normalize-json-bool (%json-dict-ref obj "willRetry" #f))
        (%json-dict-ref obj "cost" #f)
        (%json-dict-ref obj "savings" #f)
      ) ;pi-event-agent-end
    ) ;define

    (define (json->pi-event obj)
      (cond ((pi-event? obj) obj)
            ((not (pair? obj)) #f)
            (else
              (let ((etype (%json-key->string (%json-dict-ref obj "type" #f))))
                (cond ((equal? etype "session") (%decode-session obj))
                      ((equal? etype "agent_start") (pi-event-agent-start))
                      ((equal? etype "turn_start") (pi-event-turn-start))
                      ((equal? etype "message_start")
                       (pi-event-message-start (%convert-msg (%json-dict-ref obj "message" #f)))
                      ) ;
                      ((equal? etype "message_update")
                       (pi-event-message-update (%json-dict-ref obj "usage" #f)
                         (%json-dict-ref obj "assistantMessageEvent" #f)
                       ) ;pi-event-message-update
                      ) ;
                      ((equal? etype "message_end")
                       (pi-event-message-end (%convert-msg (%json-dict-ref obj "message" #f)))
                      ) ;
                      ((equal? etype "tool_execution_start")
                       (pi-event-tool-execution-start (%json-dict-ref obj "toolCallId" "")
                         (%json-dict-ref obj "toolName" "")
                         (%json-dict-ref obj "args" '(()))
                       ) ;pi-event-tool-execution-start
                      ) ;
                      ((equal? etype "tool_execution_update")
                       (pi-event-tool-execution-update (%json-dict-ref obj "toolCallId" "")
                         (%json-dict-ref obj "toolName" "")
                         (%json-dict-ref obj "args" '(()))
                         (%convert-result (%json-dict-ref obj "partialResult" #f))
                       ) ;pi-event-tool-execution-update
                      ) ;
                      ((equal? etype "tool_execution_end") (%decode-tool-execution-end obj))
                      ((equal? etype "turn_end")
                       (pi-event-turn-end (%convert-msg (%json-dict-ref obj "message" #f))
                         (%convert-list %convert-result (%json-dict-ref obj "toolResults" '()))
                       ) ;pi-event-turn-end
                      ) ;
                      ((equal? etype "agent_end") (%decode-agent-end obj))
                      ((equal? etype "agent_settled")
                       (pi-event-agent-settled (%normalize-json-bool (%json-dict-ref obj "aborted" #f))
                       ) ;pi-event-agent-settled
                      ) ;
                      (else #f)
                ) ;cond
              ) ;let
            ) ;else
      ) ;cond
    ) ;define

  ) ;begin
) ;define-library
