;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai message assistant)
  (import (scheme base) (liii base) (liii pi ai time))
  (export pi-assistant-message pi-assistant-message?
    pi-assistant-message-role pi-assistant-message-content
    pi-assistant-message-stop-reason pi-assistant-message-error-message
    pi-assistant-message-usage pi-assistant-message-timestamp
    pi-assistant-message-provider pi-assistant-message-model
    pi-assistant-message-api
  ) ;export
  (begin

    (define*-record-type <pi-assistant-message>
      (pi-assistant-message content
        (stop-reason "stop")
        (error-message #f)
        (usage #f)
        (timestamp (pi-current-time-millis))
        (provider #f)
        (model #f)
        (api #f)
      ) ;pi-assistant-message
      pi-assistant-message?
      (content pi-assistant-message-content)
      (stop-reason pi-assistant-message-stop-reason)
      (error-message pi-assistant-message-error-message)
      (usage pi-assistant-message-usage)
      (timestamp pi-assistant-message-timestamp)
      (provider pi-assistant-message-provider)
      (model pi-assistant-message-model)
      (api pi-assistant-message-api)
    ) ;define*-record-type

    (define (pi-assistant-message-role msg)
      "assistant"
    ) ;define

  ) ;begin
) ;define-library
