;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai api anthropic-messages stream-state)
  (import (scheme base))
  (export make-anthropic-stream-state anthropic-stream-state?
    anthropic-stream-state-blocks anthropic-stream-state-blocks-set!
    anthropic-stream-state-last-text anthropic-stream-state-last-text-set!
    anthropic-stream-state-usage anthropic-stream-state-usage-set!
    anthropic-stream-state-tool-calls anthropic-stream-state-tool-calls-set!
    anthropic-stream-state-stop-reason anthropic-stream-state-stop-reason-set!
    anthropic-stream-state-current-tool anthropic-stream-state-current-tool-set!
    anthropic-stream-state-current-thinking
    anthropic-stream-state-current-thinking-set!
  ) ;export
  (begin

    (define-record-type <anthropic-stream-state>
      (make-anthropic-stream-state blocks last-text usage tool-calls stop-reason
        current-tool current-thinking
      ) ;make-anthropic-stream-state
      anthropic-stream-state?
      (blocks anthropic-stream-state-record-blocks
        anthropic-stream-state-record-blocks-set!
      ) ;blocks
      (last-text anthropic-stream-state-record-last-text
        anthropic-stream-state-record-last-text-set!
      ) ;last-text
      (usage anthropic-stream-state-record-usage
        anthropic-stream-state-record-usage-set!
      ) ;usage
      (tool-calls anthropic-stream-state-record-tool-calls
        anthropic-stream-state-record-tool-calls-set!
      ) ;tool-calls
      (stop-reason anthropic-stream-state-record-stop-reason
        anthropic-stream-state-record-stop-reason-set!
      ) ;stop-reason
      (current-tool anthropic-stream-state-record-current-tool
        anthropic-stream-state-record-current-tool-set!
      ) ;current-tool
      (current-thinking anthropic-stream-state-record-current-thinking
        anthropic-stream-state-record-current-thinking-set!
      ) ;current-thinking
    ) ;define-record-type

    (define (anthropic-stream-state-blocks state)
      (if (anthropic-stream-state? state)
        (anthropic-stream-state-record-blocks state)
        (vector-ref state 0)
      ) ;if
    ) ;define

    (define (anthropic-stream-state-blocks-set! state value)
      (if (anthropic-stream-state? state)
        (anthropic-stream-state-record-blocks-set! state value)
        (vector-set! state 0 value)
      ) ;if
    ) ;define

    (define (anthropic-stream-state-last-text state)
      (if (anthropic-stream-state? state)
        (anthropic-stream-state-record-last-text state)
        (vector-ref state 1)
      ) ;if
    ) ;define

    (define (anthropic-stream-state-last-text-set! state value)
      (if (anthropic-stream-state? state)
        (anthropic-stream-state-record-last-text-set! state value)
        (vector-set! state 1 value)
      ) ;if
    ) ;define

    (define (anthropic-stream-state-usage state)
      (if (anthropic-stream-state? state)
        (anthropic-stream-state-record-usage state)
        (vector-ref state 2)
      ) ;if
    ) ;define

    (define (anthropic-stream-state-usage-set! state value)
      (if (anthropic-stream-state? state)
        (anthropic-stream-state-record-usage-set! state value)
        (vector-set! state 2 value)
      ) ;if
    ) ;define

    (define (anthropic-stream-state-tool-calls state)
      (if (anthropic-stream-state? state)
        (anthropic-stream-state-record-tool-calls state)
        (vector-ref state 3)
      ) ;if
    ) ;define

    (define (anthropic-stream-state-tool-calls-set! state value)
      (if (anthropic-stream-state? state)
        (anthropic-stream-state-record-tool-calls-set! state value)
        (vector-set! state 3 value)
      ) ;if
    ) ;define

    (define (anthropic-stream-state-stop-reason state)
      (if (anthropic-stream-state? state)
        (anthropic-stream-state-record-stop-reason state)
        (vector-ref state 4)
      ) ;if
    ) ;define

    (define (anthropic-stream-state-stop-reason-set! state value)
      (if (anthropic-stream-state? state)
        (anthropic-stream-state-record-stop-reason-set! state value)
        (vector-set! state 4 value)
      ) ;if
    ) ;define

    (define (anthropic-stream-state-current-tool state)
      (if (anthropic-stream-state? state)
        (anthropic-stream-state-record-current-tool state)
        (vector-ref state 5)
      ) ;if
    ) ;define

    (define (anthropic-stream-state-current-tool-set! state value)
      (if (anthropic-stream-state? state)
        (anthropic-stream-state-record-current-tool-set! state value)
        (vector-set! state 5 value)
      ) ;if
    ) ;define

    (define (anthropic-stream-state-current-thinking state)
      (if (anthropic-stream-state? state)
        (anthropic-stream-state-record-current-thinking state)
        (vector-ref state 6)
      ) ;if
    ) ;define

    (define (anthropic-stream-state-current-thinking-set! state value)
      (if (anthropic-stream-state? state)
        (anthropic-stream-state-record-current-thinking-set! state value)
        (vector-set! state 6 value)
      ) ;if
    ) ;define

  ) ;begin
) ;define-library
