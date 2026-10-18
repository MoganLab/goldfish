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

(define-library (liii pi event types)
  (import (scheme base))
  (export %make-pi-event-session pi-event-session? pi-event-session-version
    pi-event-session-id pi-event-session-timestamp pi-event-session-cwd
    %make-pi-event-agent-start pi-event-agent-start? %make-pi-event-turn-start
    pi-event-turn-start? %make-pi-event-message-start pi-event-message-start?
    pi-event-message-start-message %make-pi-event-message-update
    pi-event-message-update? pi-event-message-update-usage
    pi-event-message-update-assistant-message-event %make-pi-event-message-end
    pi-event-message-end? pi-event-message-end-message
    %make-pi-event-tool-execution-start pi-event-tool-execution-start?
    pi-event-tool-execution-start-tool-call-id
    pi-event-tool-execution-start-tool-name pi-event-tool-execution-start-args
    %make-pi-event-tool-execution-update pi-event-tool-execution-update?
    pi-event-tool-execution-update-tool-call-id
    pi-event-tool-execution-update-tool-name pi-event-tool-execution-update-args
    pi-event-tool-execution-update-partial-result
    %make-pi-event-tool-execution-end pi-event-tool-execution-end?
    pi-event-tool-execution-end-tool-call-id
    pi-event-tool-execution-end-tool-name pi-event-tool-execution-end-result
    pi-event-tool-execution-end-is-error %make-pi-event-turn-end
    pi-event-turn-end? pi-event-turn-end-message pi-event-turn-end-tool-results
    %make-pi-event-agent-end pi-event-agent-end? pi-event-agent-end-messages
    pi-event-agent-end-will-retry %make-pi-event-agent-settled
    pi-event-agent-settled? pi-event-agent-settled-aborted
  ) ;export
  (begin

    (define-record-type <pi-event-session>
      (%make-pi-event-session version id timestamp cwd)
      pi-event-session?
      (version pi-event-session-version)
      (id pi-event-session-id)
      (timestamp pi-event-session-timestamp)
      (cwd pi-event-session-cwd)
    ) ;define-record-type

    (define-record-type <pi-event-agent-start>
      (%make-pi-event-agent-start)
      pi-event-agent-start?
    ) ;define-record-type

    (define-record-type <pi-event-turn-start>
      (%make-pi-event-turn-start)
      pi-event-turn-start?
    ) ;define-record-type

    (define-record-type <pi-event-message-start>
      (%make-pi-event-message-start message)
      pi-event-message-start?
      (message pi-event-message-start-message)
    ) ;define-record-type

    (define-record-type <pi-event-message-update>
      (%make-pi-event-message-update usage assistant-message-event)
      pi-event-message-update?
      (usage pi-event-message-update-usage)
      (assistant-message-event pi-event-message-update-assistant-message-event)
    ) ;define-record-type

    (define-record-type <pi-event-message-end>
      (%make-pi-event-message-end message)
      pi-event-message-end?
      (message pi-event-message-end-message)
    ) ;define-record-type

    (define-record-type <pi-event-tool-execution-start>
      (%make-pi-event-tool-execution-start tool-call-id tool-name args)
      pi-event-tool-execution-start?
      (tool-call-id pi-event-tool-execution-start-tool-call-id)
      (tool-name pi-event-tool-execution-start-tool-name)
      (args pi-event-tool-execution-start-args)
    ) ;define-record-type

    (define-record-type <pi-event-tool-execution-update>
      (%make-pi-event-tool-execution-update tool-call-id tool-name args
        partial-result
      ) ;%make-pi-event-tool-execution-update
      pi-event-tool-execution-update?
      (tool-call-id pi-event-tool-execution-update-tool-call-id)
      (tool-name pi-event-tool-execution-update-tool-name)
      (args pi-event-tool-execution-update-args)
      (partial-result pi-event-tool-execution-update-partial-result)
    ) ;define-record-type

    (define-record-type <pi-event-tool-execution-end>
      (%make-pi-event-tool-execution-end tool-call-id tool-name result is-error)
      pi-event-tool-execution-end?
      (tool-call-id pi-event-tool-execution-end-tool-call-id)
      (tool-name pi-event-tool-execution-end-tool-name)
      (result pi-event-tool-execution-end-result)
      (is-error pi-event-tool-execution-end-is-error)
    ) ;define-record-type

    (define-record-type <pi-event-turn-end>
      (%make-pi-event-turn-end message tool-results)
      pi-event-turn-end?
      (message pi-event-turn-end-message)
      (tool-results pi-event-turn-end-tool-results)
    ) ;define-record-type

    (define-record-type <pi-event-agent-end>
      (%make-pi-event-agent-end messages will-retry)
      pi-event-agent-end?
      (messages pi-event-agent-end-messages)
      (will-retry pi-event-agent-end-will-retry)
    ) ;define-record-type

    (define-record-type <pi-event-agent-settled>
      (%make-pi-event-agent-settled aborted)
      pi-event-agent-settled?
      (aborted pi-event-agent-settled-aborted)
    ) ;define-record-type

  ) ;begin
) ;define-library
