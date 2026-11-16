;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai message image-block)
  (import (scheme base) (liii base))
  (export pi-image-block pi-image-block? pi-image-block-type pi-image-block-data
    pi-image-block-mime-type
  ) ;export
  (begin

    (define*-record-type <pi-image-block>
      (pi-image-block data mime-type)
      pi-image-block?
      (data pi-image-block-data)
      (mime-type pi-image-block-mime-type)
    ) ;define*-record-type

    (define (pi-image-block-type blk)
      "image"
    ) ;define

  ) ;begin
) ;define-library
