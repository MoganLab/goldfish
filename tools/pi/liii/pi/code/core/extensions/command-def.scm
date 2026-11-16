;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code core extensions command-def)
  (import (scheme base) (liii base) (liii error))
  (export make-extension-command-def extension-command-def?
    extension-command-def-name extension-command-def-description
    extension-command-def-handler extension-command-def-params
  ) ;export
  (begin

    (define*-record-type <extension-command-def>
      (make-extension-command-def name description handler (params '()))
      extension-command-def?
      (name extension-command-def-name)
      (description extension-command-def-description)
      (handler extension-command-def-handler)
      (params extension-command-def-params)
    ) ;define*-record-type

    (let ((raw make-extension-command-def))
      (set! make-extension-command-def
        (lambda* (name description handler (params '()))
          (unless (and (string? name) (> (string-length name) 0))
            (type-error "make-extension-command-def: name must be a non-empty string")
          ) ;unless
          (unless (string? description)
            (type-error "make-extension-command-def: description must be a string")
          ) ;unless
          (unless (procedure? handler)
            (type-error "make-extension-command-def: handler must be a procedure")
          ) ;unless
          (raw name description handler params)
        ) ;lambda*
      ) ;set!
    ) ;let

  ) ;begin
) ;define-library
