;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code core extensions api)
  (import (scheme base)
    (liii base)
    (liii error)
    (liii pi code core extensions extension-registry)
  ) ;import
  (export make-extension-api extension-api? extension-api-registry)
  (begin

    (define*-record-type <extension-api>
      (make-extension-api (registry #f))
      extension-api?
      (registry extension-api-registry)
    ) ;define*-record-type

    (let ((raw make-extension-api))
      (set! make-extension-api
        (lambda* ((registry #f))
          (cond ((not registry) (raw (make-extension-registry)))
                ((extension-registry? registry) (raw registry))
                (else (type-error "make-extension-api: registry must be an extension-registry"))
          ) ;cond
        ) ;lambda*
      ) ;set!
    ) ;let

  ) ;begin
) ;define-library
