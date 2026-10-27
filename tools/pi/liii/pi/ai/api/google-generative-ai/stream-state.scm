;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai api google-generative-ai stream-state)
  (import (scheme base))
  (export make-gemini-stream-state gemini-stream-state?
    gemini-stream-state-blocks gemini-stream-state-blocks-set!
    gemini-stream-state-last-text gemini-stream-state-last-text-set!
    gemini-stream-state-usage gemini-stream-state-usage-set!
    gemini-stream-state-tool-calls gemini-stream-state-tool-calls-set!
    gemini-stream-state-finish-reason gemini-stream-state-finish-reason-set!
    gemini-stream-state-current-active-type
    gemini-stream-state-current-active-type-set!
    gemini-stream-state-thinking-text gemini-stream-state-thinking-text-set!
    gemini-stream-state-thinking-signature
    gemini-stream-state-thinking-signature-set!
  ) ;export
  (begin

    (define-record-type <gemini-stream-state>
      (make-gemini-stream-state blocks last-text usage tool-calls finish-reason
        active-type thinking-text thinking-signature
      ) ;make-gemini-stream-state
      gemini-stream-state?
      (blocks gemini-stream-state-record-blocks
        gemini-stream-state-record-blocks-set!
      ) ;blocks
      (last-text gemini-stream-state-record-last-text
        gemini-stream-state-record-last-text-set!
      ) ;last-text
      (usage gemini-stream-state-record-usage gemini-stream-state-record-usage-set!)
      (tool-calls gemini-stream-state-record-tool-calls
        gemini-stream-state-record-tool-calls-set!
      ) ;tool-calls
      (finish-reason gemini-stream-state-record-finish-reason
        gemini-stream-state-record-finish-reason-set!
      ) ;finish-reason
      (active-type gemini-stream-state-record-active-type
        gemini-stream-state-record-active-type-set!
      ) ;active-type
      (thinking-text gemini-stream-state-record-thinking-text
        gemini-stream-state-record-thinking-text-set!
      ) ;thinking-text
      (thinking-signature gemini-stream-state-record-thinking-signature
        gemini-stream-state-record-thinking-signature-set!
      ) ;thinking-signature
    ) ;define-record-type

    (define (gemini-stream-state-blocks state)
      (if (gemini-stream-state? state)
        (gemini-stream-state-record-blocks state)
        (vector-ref state 0)
      ) ;if
    ) ;define

    (define (gemini-stream-state-blocks-set! state value)
      (if (gemini-stream-state? state)
        (gemini-stream-state-record-blocks-set! state value)
        (vector-set! state 0 value)
      ) ;if
    ) ;define

    (define (gemini-stream-state-last-text state)
      (if (gemini-stream-state? state)
        (gemini-stream-state-record-last-text state)
        (vector-ref state 1)
      ) ;if
    ) ;define

    (define (gemini-stream-state-last-text-set! state value)
      (if (gemini-stream-state? state)
        (gemini-stream-state-record-last-text-set! state value)
        (vector-set! state 1 value)
      ) ;if
    ) ;define

    (define (gemini-stream-state-usage state)
      (if (gemini-stream-state? state)
        (gemini-stream-state-record-usage state)
        (vector-ref state 2)
      ) ;if
    ) ;define

    (define (gemini-stream-state-usage-set! state value)
      (if (gemini-stream-state? state)
        (gemini-stream-state-record-usage-set! state value)
        (vector-set! state 2 value)
      ) ;if
    ) ;define

    (define (gemini-stream-state-tool-calls state)
      (if (gemini-stream-state? state)
        (gemini-stream-state-record-tool-calls state)
        (vector-ref state 3)
      ) ;if
    ) ;define

    (define (gemini-stream-state-tool-calls-set! state value)
      (if (gemini-stream-state? state)
        (gemini-stream-state-record-tool-calls-set! state value)
        (vector-set! state 3 value)
      ) ;if
    ) ;define

    (define (gemini-stream-state-finish-reason state)
      (if (gemini-stream-state? state)
        (gemini-stream-state-record-finish-reason state)
        (vector-ref state 4)
      ) ;if
    ) ;define

    (define (gemini-stream-state-finish-reason-set! state value)
      (if (gemini-stream-state? state)
        (gemini-stream-state-record-finish-reason-set! state value)
        (vector-set! state 4 value)
      ) ;if
    ) ;define

    (define (gemini-stream-state-current-active-type state)
      (if (gemini-stream-state? state)
        (gemini-stream-state-record-active-type state)
        (and (>= (vector-length state) 6) (vector-ref state 5))
      ) ;if
    ) ;define

    (define (gemini-stream-state-current-active-type-set! state value)
      (if (gemini-stream-state? state)
        (gemini-stream-state-record-active-type-set! state value)
        (vector-set! state 5 value)
      ) ;if
    ) ;define

    (define (gemini-stream-state-thinking-text state)
      (if (gemini-stream-state? state)
        (gemini-stream-state-record-thinking-text state)
        (vector-ref state 6)
      ) ;if
    ) ;define

    (define (gemini-stream-state-thinking-text-set! state value)
      (if (gemini-stream-state? state)
        (gemini-stream-state-record-thinking-text-set! state value)
        (vector-set! state 6 value)
      ) ;if
    ) ;define

    (define (gemini-stream-state-thinking-signature state)
      (if (gemini-stream-state? state)
        (gemini-stream-state-record-thinking-signature state)
        (vector-ref state 7)
      ) ;if
    ) ;define

    (define (gemini-stream-state-thinking-signature-set! state value)
      (if (gemini-stream-state? state)
        (gemini-stream-state-record-thinking-signature-set! state value)
        (vector-set! state 7 value)
      ) ;if
    ) ;define

  ) ;begin
) ;define-library
