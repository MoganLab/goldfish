;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code core extensions context)
  (import (scheme base)
    (liii base)
    (liii error)
    (liii pi code core extensions extension-registry)
  ) ;import
  (export make-extension-context extension-context? extension-context-session-id
    extension-context-cwd extension-context-model extension-context-registry
    extension-context-extra
  ) ;export
  (begin

    (define*-record-type <extension-context>
      (make-extension-context (session-id #f)
        (cwd ".")
        (model #f)
        (registry #f)
        (extra '())
      ) ;make-extension-context
      extension-context?
      (session-id extension-context-session-id)
      (cwd extension-context-cwd)
      (model extension-context-model)
      (registry extension-context-registry)
      (extra extension-context-extra)
    ) ;define*-record-type

    (let ((raw make-extension-context))
      (set! make-extension-context
        (lambda* ((session-id #f) (cwd ".") (model #f) (registry #f) (extra '()))
          (when (and session-id (not (string? session-id)))
            (type-error "make-extension-context: session-id must be a string or #f")
          ) ;when
          (unless (string? cwd)
            (type-error "make-extension-context: cwd must be a string")
          ) ;unless
          (when (and model (not (string? model)))
            (type-error "make-extension-context: model must be a string or #f")
          ) ;when
          (let ((reg (if registry registry (make-extension-registry))))
            (unless (extension-registry? reg)
              (type-error "make-extension-context: registry must be an extension-registry")
            ) ;unless
            (unless (list? extra)
              (type-error "make-extension-context: extra must be a list")
            ) ;unless
            (raw session-id cwd model reg extra)
          ) ;let
        ) ;lambda*
      ) ;set!
    ) ;let

  ) ;begin
) ;define-library
