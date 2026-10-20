;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi message types)
  (import (scheme base))
  (export %make-pi-user-message pi-user-message? pi-user-message-content
    pi-user-message-timestamp %make-pi-assistant-message pi-assistant-message?
    pi-assistant-message-content pi-assistant-message-stop-reason
    pi-assistant-message-error-message pi-assistant-message-usage
    pi-assistant-message-timestamp %make-pi-tool-result-message
    pi-tool-result-message? pi-tool-result-message-tool-call-id
    pi-tool-result-message-tool-name pi-tool-result-message-content
    pi-tool-result-message-is-error pi-tool-result-message-timestamp
    %make-pi-text-block pi-text-block? pi-text-block-text
    %make-pi-thinking-block pi-thinking-block? pi-thinking-block-thinking
    pi-thinking-block-thinking-signature %make-pi-tool-call-block
    pi-tool-call-block? pi-tool-call-block-id pi-tool-call-block-name
    pi-tool-call-block-arguments pi-tool-call-block-thought-signature
  ) ;export
  (begin

    (define-record-type <pi-user-message>
      (%make-pi-user-message content timestamp)
      pi-user-message?
      (content pi-user-message-content)
      (timestamp pi-user-message-timestamp)
    ) ;define-record-type

    (define-record-type <pi-assistant-message>
      (%make-pi-assistant-message content stop-reason error-message usage timestamp)
      pi-assistant-message?
      (content pi-assistant-message-content)
      (stop-reason pi-assistant-message-stop-reason)
      (error-message pi-assistant-message-error-message)
      (usage pi-assistant-message-usage)
      (timestamp pi-assistant-message-timestamp)
    ) ;define-record-type

    (define-record-type <pi-tool-result-message>
      (%make-pi-tool-result-message tool-call-id tool-name content is-error timestamp)
      pi-tool-result-message?
      (tool-call-id pi-tool-result-message-tool-call-id)
      (tool-name pi-tool-result-message-tool-name)
      (content pi-tool-result-message-content)
      (is-error pi-tool-result-message-is-error)
      (timestamp pi-tool-result-message-timestamp)
    ) ;define-record-type

    (define-record-type <pi-text-block>
      (%make-pi-text-block text)
      pi-text-block?
      (text pi-text-block-text)
    ) ;define-record-type

    (define-record-type <pi-thinking-block>
      (%make-pi-thinking-block thinking thinking-signature)
      pi-thinking-block?
      (thinking pi-thinking-block-thinking)
      (thinking-signature pi-thinking-block-thinking-signature)
    ) ;define-record-type

    (define-record-type <pi-tool-call-block>
      (%make-pi-tool-call-block id name arguments thought-signature)
      pi-tool-call-block?
      (id pi-tool-call-block-id)
      (name pi-tool-call-block-name)
      (arguments pi-tool-call-block-arguments)
      (thought-signature pi-tool-call-block-thought-signature)
    ) ;define-record-type

  ) ;begin
) ;define-library
