;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai api openai-completions stream-state)
  (import (scheme base) (liii base))
  (export make-openai-stream-state openai-stream-state?
    openai-stream-state-blocks openai-stream-state-blocks-set!
    openai-stream-state-last-text openai-stream-state-last-text-set!
    openai-stream-state-usage openai-stream-state-usage-set!
    openai-stream-state-stop-reason openai-stream-state-stop-reason-set!
    openai-stream-state-error-message openai-stream-state-error-message-set!
    openai-stream-state-tools openai-stream-state-tools-set!
    openai-stream-state-last-reasoning openai-stream-state-last-reasoning-set!
  ) ;export
  (begin

    (define*-record-type <openai-stream-state>
      (make-openai-stream-state
        (blocks '())
        (last-text #f)
        (usage '())
        (stop-reason "stop")
        (error-message #f)
        (tools '())
        (last-reasoning #f)
      ) ;make-openai-stream-state
      openai-stream-state?
      (blocks openai-stream-state-blocks openai-stream-state-blocks-set!)
      (last-text openai-stream-state-last-text openai-stream-state-last-text-set!)
      (usage openai-stream-state-usage openai-stream-state-usage-set!)
      (stop-reason openai-stream-state-stop-reason
        openai-stream-state-stop-reason-set!
      ) ;stop-reason
      (error-message openai-stream-state-error-message
        openai-stream-state-error-message-set!
      ) ;error-message
      (tools openai-stream-state-tools openai-stream-state-tools-set!)
      (last-reasoning openai-stream-state-last-reasoning
        openai-stream-state-last-reasoning-set!
      ) ;last-reasoning
    ) ;define*-record-type

  ) ;begin
) ;define-library
