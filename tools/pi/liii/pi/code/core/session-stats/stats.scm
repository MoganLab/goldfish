;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code core session-stats stats)
  (import (scheme base)
    (liii base)
    (liii pi code core session-stats token-stats)
  ) ;import
  (export make-pi-session-stats pi-session-stats?
    pi-session-stats-session-file pi-session-stats-session-id
    pi-session-stats-session-id-set! pi-session-stats-session-name
    pi-session-stats-session-name-set! pi-session-stats-total-messages
    pi-session-stats-total-messages-set! pi-session-stats-user-messages
    pi-session-stats-user-messages-set! pi-session-stats-assistant-messages
    pi-session-stats-assistant-messages-set! pi-session-stats-tool-calls
    pi-session-stats-tool-calls-set! pi-session-stats-tool-results
    pi-session-stats-tool-results-set! pi-session-stats-tokens
    pi-session-stats-cost pi-session-stats-cost-set!
  ) ;export
  (begin

    (define*-record-type <pi-session-stats>
      (make-pi-session-stats session-file
        (session-id #f)
        (session-name #f)
        (total-messages 0)
        (user-messages 0)
        (assistant-messages 0)
        (tool-calls 0)
        (tool-results 0)
        (tokens (make-pi-session-token-stats))
        (cost 0)
      ) ;make-pi-session-stats
      pi-session-stats?
      (session-file pi-session-stats-session-file)
      (session-id pi-session-stats-session-id pi-session-stats-session-id-set!)
      (session-name pi-session-stats-session-name pi-session-stats-session-name-set!)
      (total-messages pi-session-stats-total-messages
        pi-session-stats-total-messages-set!
      ) ;total-messages
      (user-messages pi-session-stats-user-messages
        pi-session-stats-user-messages-set!
      ) ;user-messages
      (assistant-messages pi-session-stats-assistant-messages
        pi-session-stats-assistant-messages-set!
      ) ;assistant-messages
      (tool-calls pi-session-stats-tool-calls pi-session-stats-tool-calls-set!)
      (tool-results pi-session-stats-tool-results pi-session-stats-tool-results-set!)
      (tokens pi-session-stats-tokens)
      (cost pi-session-stats-cost pi-session-stats-cost-set!)
    ) ;define*-record-type

  ) ;begin
) ;define-library
