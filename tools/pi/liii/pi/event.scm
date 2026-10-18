;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
;; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
;; License for the specific language governing permissions and limitations
;; under the License.
;;

(define-library (liii pi event)
  (import (scheme base)
    (liii json)
    (liii list)
    (liii pi time)
    (liii pi event types)
    (liii pi event record)
    (liii pi message)
    (liii pi config types)
    (liii pi config record)
    (liii pi tools types)
    (liii pi tools record)
  ) ;import
  (export pi-iso-8601-utc-now pi-current-time-millis pi-serialize-event assoc
    pi-event? pi-event-type pi-event->alist pi-event-session pi-event-session?
    pi-event-session-type pi-event-session-version pi-event-session-id
    pi-event-session-timestamp pi-event-session-cwd pi-event-agent-start
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
    pi-event-tool-execution-end-is-error pi-event-turn-end pi-event-turn-end?
    pi-event-turn-end-type pi-event-turn-end-message
    pi-event-turn-end-tool-results pi-event-agent-end pi-event-agent-end?
    pi-event-agent-end-type pi-event-agent-end-messages
    pi-event-agent-end-will-retry pi-event-agent-settled pi-event-agent-settled?
    pi-event-agent-settled-type pi-event-agent-settled-aborted
    pi-build-json-event
  ) ;export
  (begin

    (define %orig-assoc assoc)

    (define (%key-match? k entry-k)
      (let ((norm-k (if (or (equal? k 'is-error) (equal? k "is-error")) 'isError k))
            (norm-ek (if (or (equal? entry-k 'is-error) (equal? entry-k "is-error"))
                       'isError
                       entry-k
                     ) ;if
            ) ;norm-ek
           ) ;
        (or (equal? norm-ek norm-k)
          (and (symbol? norm-ek)
            (string? norm-k)
            (string=? (symbol->string norm-ek) norm-k)
          ) ;and
          (and (string? norm-ek)
            (symbol? norm-k)
            (string=? norm-ek (symbol->string norm-k))
          ) ;and
        ) ;or
      ) ;let
    ) ;define

    (define (%find-assoc key lst)
      (let loop
        ((entries lst))
        (cond ((null? entries) #f)
              ((not (pair? entries)) #f)
              ((pi-credential? (car entries))
               (if (%key-match? key (pi-credential-provider (car entries)))
                 (cons (pi-credential-provider (car entries)) (pi-credential-key (car entries)))
                 (loop (cdr entries))
               ) ;if
              ) ;
              ((and (pair? (car entries)) (%key-match? key (caar entries))) (car entries))
              (else (loop (cdr entries)))
        ) ;cond
      ) ;let
    ) ;define

    (define (assoc key obj)
      (cond ((pi-event? obj) (%find-assoc key (pi-event->alist obj)))
            ((pi-message? obj) (%find-assoc key (pi-message->alist obj)))
            ((pi-block? obj) (%find-assoc key (pi-block->alist obj)))
            ((pi-retry-config? obj) (%find-assoc key (pi-retry-config->alist obj)))
            ((pi-settings? obj) (%find-assoc key (pi-settings->alist obj)))
            ((pi-credential? obj) (%find-assoc key (pi-credential->alist obj)))
            ((pi-model-metadata? obj) (%find-assoc key (pi-model-metadata->alist obj)))
            ((pi-tool-definition? obj) (%find-assoc key (pi-tool-definition->alist obj)))
            ((pi-tool-result? obj) (%find-assoc key (pi-tool-result->alist obj)))
            ((pi-trunc-result? obj) (%find-assoc key (pi-trunc-result->alist obj)))
            ((pair? obj) (%find-assoc key obj))
            (else (%orig-assoc key obj))
      ) ;cond
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
       ((agent_end) (pi-event-agent-end (car args) (cadr args)))
       ((agent_settled) (pi-event-agent-settled (car args)))
       (else #f)
      ) ;case
    ) ;define

    (define (%json-object-entry? x)
      (and (pair? x) (not (pair? (car x))) (or (symbol? (car x)) (string? (car x))))
    ) ;define

    (define (%normalize-json-value x)
      (cond ((null? x) #())
            ((pi-event? x) (%normalize-json-value (pi-event->alist x)))
            ((pi-message? x) (%normalize-json-value (pi-message->alist x)))
            ((pi-block? x) (%normalize-json-value (pi-block->alist x)))
            ((pi-tool-result? x) (%normalize-json-value (pi-tool-result->alist x)))
            ((pi-tool-definition? x) (%normalize-json-value (pi-tool-definition->alist x)))
            ((pi-trunc-result? x) (%normalize-json-value (pi-trunc-result->alist x)))
            ((vector? x) (list->vector (map %normalize-json-value (vector->list x))))
            ((and (pair? x) (every %json-object-entry? x))
             (let loop
               ((entries x) (acc '()))
               (if (null? entries)
                 (reverse acc)
                 (let* ((entry (car entries))
                        (raw-k (car entry))
                        (k (if (symbol? raw-k) (symbol->string raw-k) raw-k))
                        (v (cdr entry))
                       ) ;
                   (if (and (string=? k "errorMessage") (not v))
                     (loop (cdr entries) acc)
                     (loop (cdr entries) (cons (cons k (%normalize-json-value v)) acc))
                   ) ;if
                 ) ;let*
               ) ;if
             ) ;let
            ) ;
            ((pair? x) (list->vector (map %normalize-json-value x)))
            ((symbol? x) (symbol->string x))
            (else x)
      ) ;cond
    ) ;define

    (define (pi-serialize-event alist)
      (json->string (%normalize-json-value alist))
    ) ;define

  ) ;begin
) ;define-library
