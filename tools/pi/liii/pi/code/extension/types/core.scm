;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code extension types core)
  (import (scheme base) (liii error))
  (export make-extension-registry
    extension-registry?
    extension-registry-tools
    extension-registry-flags
    extension-registry-commands
    extension-registry-listeners
    extension-registry-flows
    extension-registry-validators
    extension-registry-augmenters
    extension-registry-prompt-injectors
    set-extension-registry-tools!
    set-extension-registry-flags!
    set-extension-registry-commands!
    set-extension-registry-listeners!
    set-extension-registry-flows!
    set-extension-registry-validators!
    set-extension-registry-augmenters!
    set-extension-registry-prompt-injectors!

    make-extension-api
    extension-api?
    extension-api-registry

    make-extension-context
    extension-context?
    extension-context-session-id
    extension-context-cwd
    extension-context-model
    extension-context-registry
    extension-context-extra
  ) ;export
  (begin

    ;; 1. ExtensionRegistry
    (define-record-type <extension-registry>
      (%make-extension-registry tools flags commands listeners flows validators
        augmenters prompt-injectors
      ) ;%make-extension-registry
      extension-registry?
      (tools extension-registry-tools set-extension-registry-tools!)
      (flags extension-registry-flags set-extension-registry-flags!)
      (commands extension-registry-commands set-extension-registry-commands!)
      (listeners extension-registry-listeners set-extension-registry-listeners!)
      (flows extension-registry-flows set-extension-registry-flows!)
      (validators extension-registry-validators set-extension-registry-validators!)
      (augmenters extension-registry-augmenters set-extension-registry-augmenters!)
      (prompt-injectors extension-registry-prompt-injectors
        set-extension-registry-prompt-injectors!
      ) ;prompt-injectors
    ) ;define-record-type

    (define (make-extension-registry)
      (%make-extension-registry '() '() '() '() '() '() '() '())
    ) ;define

    ;; 2. ExtensionAPI
    (define-record-type <extension-api>
      (%make-extension-api registry)
      extension-api?
      (registry extension-api-registry)
    ) ;define-record-type

    (define* (make-extension-api (registry #f))
      (cond ((not registry) (%make-extension-api (make-extension-registry)))
            ((extension-registry? registry) (%make-extension-api registry))
            (else (type-error "make-extension-api: registry must be an extension-registry"))
      ) ;cond
    ) ;define*

    ;; 3. ExtensionContext
    (define-record-type <extension-context>
      (%make-extension-context session-id cwd model registry extra)
      extension-context?
      (session-id extension-context-session-id)
      (cwd extension-context-cwd)
      (model extension-context-model)
      (registry extension-context-registry)
      (extra extension-context-extra)
    ) ;define-record-type

    (define* (make-extension-context (session-id #f)
               (cwd ".")
               (model #f)
               (registry #f)
               (extra '())
             ) ;make-extension-context
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
        (%make-extension-context session-id cwd model reg extra)
      ) ;let
    ) ;define*

  ) ;begin
) ;define-library
