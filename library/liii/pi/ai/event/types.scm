;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai event types)
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
    pi-event-tool-execution-end-is-error pi-event-tool-execution-end-duration-ms
    %make-pi-event-turn-end pi-event-turn-end? pi-event-turn-end-message
    pi-event-turn-end-tool-results %make-pi-event-agent-end pi-event-agent-end?
    pi-event-agent-end-messages pi-event-agent-end-will-retry
    pi-event-agent-end-cost pi-event-agent-end-savings
    %make-pi-event-agent-settled pi-event-agent-settled?
    pi-event-agent-settled-aborted
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
      (%make-pi-event-tool-execution-end tool-call-id tool-name result is-error
        duration-ms
      ) ;%make-pi-event-tool-execution-end
      pi-event-tool-execution-end?
      (tool-call-id pi-event-tool-execution-end-tool-call-id)
      (tool-name pi-event-tool-execution-end-tool-name)
      (result pi-event-tool-execution-end-result)
      (is-error pi-event-tool-execution-end-is-error)
      (duration-ms pi-event-tool-execution-end-duration-ms)
    ) ;define-record-type

    (define-record-type <pi-event-turn-end>
      (%make-pi-event-turn-end message tool-results)
      pi-event-turn-end?
      (message pi-event-turn-end-message)
      (tool-results pi-event-turn-end-tool-results)
    ) ;define-record-type

    (define-record-type <pi-event-agent-end>
      (%make-pi-event-agent-end messages will-retry cost savings)
      pi-event-agent-end?
      (messages pi-event-agent-end-messages)
      (will-retry pi-event-agent-end-will-retry)
      (cost pi-event-agent-end-cost)
      (savings pi-event-agent-end-savings)
    ) ;define-record-type

    (define-record-type <pi-event-agent-settled>
      (%make-pi-event-agent-settled aborted)
      pi-event-agent-settled?
      (aborted pi-event-agent-settled-aborted)
    ) ;define-record-type

  ) ;begin
) ;define-library
