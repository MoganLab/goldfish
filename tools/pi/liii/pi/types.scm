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

(define-library (liii pi types)
  (import (scheme base) (liii base) (liii pi event) (liii pi message))
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
    pi-build-json-event pi-make-user-message pi-make-assistant-message
    pi-make-tool-result-message pi-make-usage pi-message? pi-message-role
    pi-message->alist pi-block? pi-block-type pi-block->alist pi-user-message
    pi-user-message? pi-user-message-role pi-user-message-content
    pi-user-message-timestamp pi-assistant-message pi-assistant-message?
    pi-assistant-message-role pi-assistant-message-content
    pi-assistant-message-stop-reason pi-assistant-message-error-message
    pi-assistant-message-usage pi-assistant-message-timestamp
    pi-tool-result-message pi-tool-result-message? pi-tool-result-message-role
    pi-tool-result-message-tool-call-id pi-tool-result-message-tool-name
    pi-tool-result-message-content pi-tool-result-message-is-error
    pi-tool-result-message-timestamp pi-text-block pi-text-block?
    pi-text-block-type pi-text-block-text pi-thinking-block pi-thinking-block?
    pi-thinking-block-type pi-thinking-block-thinking
    pi-thinking-block-thinking-signature pi-tool-call-block pi-tool-call-block?
    pi-tool-call-block-type pi-tool-call-block-id pi-tool-call-block-name
    pi-tool-call-block-arguments pi-tool-call-block-thought-signature
  ) ;export
  (begin

    (define (pi-make-user-message text)
      (pi-user-message text)
    ) ;define

    (define* (pi-make-assistant-message content (stop-reason "stop") (error-msg #f))
      (pi-assistant-message content :stop-reason stop-reason :error-message error-msg)
    ) ;define*

    (define (pi-make-tool-result-message tool-call-id tool-name content is-error)
      (pi-tool-result-message tool-call-id tool-name content is-error)
    ) ;define

    (define* (pi-make-usage (input 0) (output 0) (total 0))
      (list (cons 'input input) (cons 'output output) (cons 'total total))
    ) ;define*

  ) ;begin
) ;define-library
