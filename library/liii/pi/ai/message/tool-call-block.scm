;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai message tool-call-block)
  (import (scheme base) (liii base))
  (export pi-tool-call-block pi-tool-call-block? pi-tool-call-block-type
    pi-tool-call-block-id pi-tool-call-block-name pi-tool-call-block-arguments
    pi-tool-call-block-thought-signature
  ) ;export
  (begin

    (define*-record-type <pi-tool-call-block>
      (pi-tool-call-block id name arguments (thought-signature #f))
      pi-tool-call-block?
      (id pi-tool-call-block-id)
      (name pi-tool-call-block-name)
      (arguments pi-tool-call-block-arguments)
      (thought-signature pi-tool-call-block-thought-signature)
    ) ;define*-record-type

    (define (pi-tool-call-block-type blk)
      "toolCall"
    ) ;define

  ) ;begin
) ;define-library
