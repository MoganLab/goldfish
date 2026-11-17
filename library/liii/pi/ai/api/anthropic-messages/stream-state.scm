;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai api anthropic-messages stream-state)
  (import (scheme base) (liii base))
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

    (define*-record-type <anthropic-stream-state>
      (make-anthropic-stream-state (blocks '())
        (last-text #f)
        (usage '())
        (tool-calls '())
        (stop-reason "end_turn")
        (current-tool #f)
        (current-thinking #f)
      ) ;make-anthropic-stream-state
      anthropic-stream-state?
      (blocks anthropic-stream-state-blocks anthropic-stream-state-blocks-set!)
      (last-text anthropic-stream-state-last-text
        anthropic-stream-state-last-text-set!
      ) ;last-text
      (usage anthropic-stream-state-usage anthropic-stream-state-usage-set!)
      (tool-calls anthropic-stream-state-tool-calls
        anthropic-stream-state-tool-calls-set!
      ) ;tool-calls
      (stop-reason anthropic-stream-state-stop-reason
        anthropic-stream-state-stop-reason-set!
      ) ;stop-reason
      (current-tool anthropic-stream-state-current-tool
        anthropic-stream-state-current-tool-set!
      ) ;current-tool
      (current-thinking anthropic-stream-state-current-thinking
        anthropic-stream-state-current-thinking-set!
      ) ;current-thinking
    ) ;define*-record-type

  ) ;begin
) ;define-library
