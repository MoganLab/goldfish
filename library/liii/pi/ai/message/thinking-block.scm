;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai message thinking-block)
  (import (scheme base) (liii base))
  (export pi-thinking-block pi-thinking-block? pi-thinking-block-type
    pi-thinking-block-thinking pi-thinking-block-thinking-signature
  ) ;export
  (begin

    (define*-record-type <pi-thinking-block>
      (pi-thinking-block thinking (thinking-signature #f))
      pi-thinking-block?
      (thinking pi-thinking-block-thinking)
      (thinking-signature pi-thinking-block-thinking-signature)
    ) ;define*-record-type

    (define (pi-thinking-block-type blk)
      "thinking"
    ) ;define

  ) ;begin
) ;define-library
