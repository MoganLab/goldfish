;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai api openai-completions stream-state)
  (import (scheme base))
  (export make-openai-stream-state openai-stream-state?
    openai-stream-state-blocks openai-stream-state-blocks-set!
    openai-stream-state-last-text openai-stream-state-last-text-set!
    openai-stream-state-usage openai-stream-state-usage-set!
    openai-stream-state-stop-reason openai-stream-state-stop-reason-set!
    openai-stream-state-error-message openai-stream-state-error-message-set!
    openai-stream-state-tools openai-stream-state-tools-set!
  ) ;export
  (begin

    (define-record-type <openai-stream-state>
      (make-openai-stream-state blocks last-text usage stop-reason error-message
        tools
      ) ;make-openai-stream-state
      openai-stream-state?
      (blocks openai-stream-state-record-blocks
        openai-stream-state-record-blocks-set!
      ) ;blocks
      (last-text openai-stream-state-record-last-text
        openai-stream-state-record-last-text-set!
      ) ;last-text
      (usage openai-stream-state-record-usage openai-stream-state-record-usage-set!)
      (stop-reason openai-stream-state-record-stop-reason
        openai-stream-state-record-stop-reason-set!
      ) ;stop-reason
      (error-message openai-stream-state-record-error-message
        openai-stream-state-record-error-message-set!
      ) ;error-message
      (tools openai-stream-state-record-tools openai-stream-state-record-tools-set!)
    ) ;define-record-type

    (define (openai-stream-state-blocks state)
      (if (openai-stream-state? state)
        (openai-stream-state-record-blocks state)
        (vector-ref state 0)
      ) ;if
    ) ;define

    (define (openai-stream-state-blocks-set! state value)
      (if (openai-stream-state? state)
        (openai-stream-state-record-blocks-set! state value)
        (vector-set! state 0 value)
      ) ;if
    ) ;define

    (define (openai-stream-state-last-text state)
      (if (openai-stream-state? state)
        (openai-stream-state-record-last-text state)
        (vector-ref state 1)
      ) ;if
    ) ;define

    (define (openai-stream-state-last-text-set! state value)
      (if (openai-stream-state? state)
        (openai-stream-state-record-last-text-set! state value)
        (vector-set! state 1 value)
      ) ;if
    ) ;define

    (define (openai-stream-state-usage state)
      (if (openai-stream-state? state)
        (openai-stream-state-record-usage state)
        (vector-ref state 2)
      ) ;if
    ) ;define

    (define (openai-stream-state-usage-set! state value)
      (if (openai-stream-state? state)
        (openai-stream-state-record-usage-set! state value)
        (vector-set! state 2 value)
      ) ;if
    ) ;define

    (define (openai-stream-state-stop-reason state)
      (if (openai-stream-state? state)
        (openai-stream-state-record-stop-reason state)
        (vector-ref state 3)
      ) ;if
    ) ;define

    (define (openai-stream-state-stop-reason-set! state value)
      (if (openai-stream-state? state)
        (openai-stream-state-record-stop-reason-set! state value)
        (vector-set! state 3 value)
      ) ;if
    ) ;define

    (define (openai-stream-state-error-message state)
      (if (openai-stream-state? state)
        (openai-stream-state-record-error-message state)
        (vector-ref state 4)
      ) ;if
    ) ;define

    (define (openai-stream-state-error-message-set! state value)
      (if (openai-stream-state? state)
        (openai-stream-state-record-error-message-set! state value)
        (vector-set! state 4 value)
      ) ;if
    ) ;define

    (define (openai-stream-state-tools state)
      (if (openai-stream-state? state)
        (openai-stream-state-record-tools state)
        (vector-ref state 5)
      ) ;if
    ) ;define

    (define (openai-stream-state-tools-set! state value)
      (if (openai-stream-state? state)
        (openai-stream-state-record-tools-set! state value)
        (vector-set! state 5 value)
      ) ;if
    ) ;define

  ) ;begin
) ;define-library
