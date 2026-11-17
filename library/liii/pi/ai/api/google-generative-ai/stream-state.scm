;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai api google-generative-ai stream-state)
  (import (scheme base) (liii base))
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
    gemini-stream-state-text-signature gemini-stream-state-text-signature-set!
  ) ;export
  (begin

    (define*-record-type <gemini-stream-state>
      (make-gemini-stream-state
        (blocks '())
        (last-text #f)
        (usage '())
        (tool-calls '())
        (finish-reason #f)
        (active-type #f)
        (thinking-text #f)
        (thinking-signature #f)
        (text-signature #f)
      ) ;make-gemini-stream-state
      gemini-stream-state?
      (blocks gemini-stream-state-blocks gemini-stream-state-blocks-set!)
      (last-text gemini-stream-state-last-text gemini-stream-state-last-text-set!)
      (usage gemini-stream-state-usage gemini-stream-state-usage-set!)
      (tool-calls gemini-stream-state-tool-calls
        gemini-stream-state-tool-calls-set!
      ) ;tool-calls
      (finish-reason gemini-stream-state-finish-reason
        gemini-stream-state-finish-reason-set!
      ) ;finish-reason
      (active-type gemini-stream-state-current-active-type
        gemini-stream-state-current-active-type-set!
      ) ;active-type
      (thinking-text gemini-stream-state-thinking-text
        gemini-stream-state-thinking-text-set!
      ) ;thinking-text
      (thinking-signature gemini-stream-state-thinking-signature
        gemini-stream-state-thinking-signature-set!
      ) ;thinking-signature
      (text-signature gemini-stream-state-text-signature
        gemini-stream-state-text-signature-set!
      ) ;text-signature
    ) ;define*-record-type

  ) ;begin
) ;define-library
