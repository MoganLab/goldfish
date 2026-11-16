;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai types)
  (import (scheme base) (liii base) (liii pi ai time) (liii pi ai message))
  (export pi-iso-8601-utc-now pi-current-time-millis pi-make-user-message
    pi-make-assistant-message pi-make-tool-result-message pi-make-usage
    pi-message? pi-message-role pi-message->json json->pi-message pi-block?
    pi-block-type pi-block->json json->pi-block pi-user-message pi-user-message?
    pi-user-message-role pi-user-message-content pi-user-message-timestamp
    pi-assistant-message pi-assistant-message? pi-assistant-message-role
    pi-assistant-message-content pi-assistant-message-stop-reason
    pi-assistant-message-error-message pi-assistant-message-usage
    pi-assistant-message-timestamp pi-assistant-message-provider
    pi-assistant-message-model pi-assistant-message-api pi-tool-result-message
    pi-tool-result-message? pi-tool-result-message-role
    pi-tool-result-message-tool-call-id pi-tool-result-message-tool-name
    pi-tool-result-message-content pi-tool-result-message-is-error
    pi-tool-result-message-timestamp pi-text-block pi-text-block?
    pi-text-block-type pi-text-block-text pi-thinking-block pi-thinking-block?
    pi-thinking-block-type pi-thinking-block-thinking
    pi-thinking-block-thinking-signature pi-tool-call-block pi-tool-call-block?
    pi-tool-call-block-type pi-tool-call-block-id pi-tool-call-block-name
    pi-tool-call-block-arguments pi-tool-call-block-thought-signature
    pi-image-block pi-image-block? pi-image-block-type pi-image-block-data
    pi-image-block-mime-type
  ) ;export
  (begin

    (define (pi-make-user-message text)
      (pi-user-message text)
    ) ;define

    (define* (pi-make-assistant-message content
               (stop-reason "stop")
               (error-msg #f)
               (provider #f)
               (model #f)
               (api #f)
             ) ;pi-make-assistant-message
      (pi-assistant-message content
        :stop-reason   stop-reason
        :error-message error-msg
        :provider      provider
        :model         model
        :api           api
      ) ;pi-assistant-message
    ) ;define*

    (define (pi-make-tool-result-message tool-call-id tool-name content is-error)
      (pi-tool-result-message tool-call-id tool-name content is-error)
    ) ;define

    (define* (pi-make-usage (input 0) (output 0) (total 0))
      (list (cons 'input input) (cons 'output output) (cons 'total total))
    ) ;define*

  ) ;begin
) ;define-library
