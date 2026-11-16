;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code core extensions tool-def)
  (import (scheme base) (liii base) (liii error))
  (export make-extension-tool-def extension-tool-def? extension-tool-def-name
    extension-tool-def-description extension-tool-def-parameters
    extension-tool-def-execute extension-tool-def-snippet
    extension-tool-def-guidelines extension-tool-def-default-active?
  ) ;export
  (begin

    (define*-record-type <extension-tool-def>
      (make-extension-tool-def name
        description
        parameters
        execute
        (snippet "")
        (guidelines '())
        (default-active? #f)
      ) ;make-extension-tool-def
      extension-tool-def?
      (name extension-tool-def-name)
      (description extension-tool-def-description)
      (parameters extension-tool-def-parameters)
      (execute extension-tool-def-execute)
      (snippet extension-tool-def-snippet)
      (guidelines extension-tool-def-guidelines)
      (default-active? extension-tool-def-default-active?)
    ) ;define*-record-type

    (let ((raw make-extension-tool-def))
      (set! make-extension-tool-def
        (lambda* (name description
                   parameters
                   execute
                   (snippet "")
                   (guidelines '())
                   (default-active? #f)
                 ) ;name
          (unless (and (string? name) (> (string-length name) 0))
            (type-error "make-extension-tool-def: name must be a non-empty string")
          ) ;unless
          (unless (string? description)
            (type-error "make-extension-tool-def: description must be a string")
          ) ;unless
          (unless (procedure? execute)
            (type-error "make-extension-tool-def: execute must be a procedure")
          ) ;unless
          (raw name
            description
            parameters
            execute
            snippet
            guidelines
            (and default-active? #t)
          ) ;raw
        ) ;lambda*
      ) ;set!
    ) ;let

  ) ;begin
) ;define-library
