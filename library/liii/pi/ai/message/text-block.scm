;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai message text-block)
  (import (scheme base) (liii base))
  (export pi-text-block pi-text-block? pi-text-block-type pi-text-block-text
    pi-text-block-signature
  ) ;export
  (begin

    (define*-record-type <pi-text-block>
      (pi-text-block text (signature #f))
      pi-text-block?
      (text pi-text-block-text)
      (signature pi-text-block-signature)
    ) ;define*-record-type

    (define (pi-text-block-type blk)
      "text"
    ) ;define

  ) ;begin
) ;define-library
