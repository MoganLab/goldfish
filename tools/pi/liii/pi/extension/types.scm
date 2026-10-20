;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi extension types)
  (import (scheme base) (liii error))
  (export
    ;; ExtensionRegistry
    make-extension-registry
    extension-registry?
    extension-registry-tools
    extension-registry-flags
    extension-registry-commands
    extension-registry-listeners
    set-extension-registry-tools!
    set-extension-registry-flags!
    set-extension-registry-commands!
    set-extension-registry-listeners!

    ;; ExtensionAPI
    make-extension-api
    extension-api?
    extension-api-registry

    ;; ExtensionContext
    make-extension-context
    extension-context?
    extension-context-session-id
    extension-context-cwd
    extension-context-model
    extension-context-registry
    extension-context-extra

    ;; ExtensionToolDef
    make-extension-tool-def
    extension-tool-def?
    extension-tool-def-name
    extension-tool-def-description
    extension-tool-def-parameters
    extension-tool-def-execute
    extension-tool-def-snippet
    extension-tool-def-guidelines

    ;; ExtensionFlagDef
    make-extension-flag-def
    extension-flag-def?
    extension-flag-def-flag
    extension-flag-def-description
    extension-flag-def-handler
    extension-flag-def-type
    extension-flag-def-default

    ;; ExtensionCommandDef
    make-extension-command-def
    extension-command-def?
    extension-command-def-name
    extension-command-def-description
    extension-command-def-handler
    extension-command-def-params
  ) ;export
  (begin

    ;; 1. ExtensionRegistry
    (define-record-type <extension-registry>
      (%make-extension-registry tools flags commands listeners)
      extension-registry?
      (tools extension-registry-tools set-extension-registry-tools!)
      (flags extension-registry-flags set-extension-registry-flags!)
      (commands extension-registry-commands set-extension-registry-commands!)
      (listeners extension-registry-listeners set-extension-registry-listeners!)
    ) ;define-record-type

    (define (make-extension-registry)
      (%make-extension-registry '() '() '() '())
    ) ;define

    ;; 2. ExtensionAPI
    (define-record-type <extension-api>
      (%make-extension-api registry)
      extension-api?
      (registry extension-api-registry)
    ) ;define-record-type

    (define* (make-extension-api (registry #f))
      (if (not registry)
        (%make-extension-api (make-extension-registry))
        (if (extension-registry? registry)
          (%make-extension-api registry)
          (type-error "make-extension-api: registry must be an extension-registry")
        ) ;if
      ) ;if
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
      (when (not (string? cwd))
        (type-error "make-extension-context: cwd must be a string")
      ) ;when
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

    ;; 4. ExtensionToolDef
    (define-record-type <extension-tool-def>
      (%make-extension-tool-def name description parameters execute snippet
        guidelines
      ) ;%make-extension-tool-def
      extension-tool-def?
      (name extension-tool-def-name)
      (description extension-tool-def-description)
      (parameters extension-tool-def-parameters)
      (execute extension-tool-def-execute)
      (snippet extension-tool-def-snippet)
      (guidelines extension-tool-def-guidelines)
    ) ;define-record-type

    (define* (make-extension-tool-def name
               description
               parameters
               execute
               (snippet "")
               (guidelines '())
             ) ;make-extension-tool-def
      (unless (and (string? name) (> (string-length name) 0))
        (type-error "make-extension-tool-def: name must be a non-empty string")
      ) ;unless
      (unless (string? description)
        (type-error "make-extension-tool-def: description must be a string")
      ) ;unless
      (unless (procedure? execute)
        (type-error "make-extension-tool-def: execute must be a procedure")
      ) ;unless
      (%make-extension-tool-def name description parameters execute snippet
        guidelines
      ) ;%make-extension-tool-def
    ) ;define*

    ;; 5. ExtensionFlagDef
    (define-record-type <extension-flag-def>
      (%make-extension-flag-def flag description handler type default)
      extension-flag-def?
      (flag extension-flag-def-flag)
      (description extension-flag-def-description)
      (handler extension-flag-def-handler)
      (type extension-flag-def-type)
      (default extension-flag-def-default)
    ) ;define-record-type

    (define* (make-extension-flag-def flag
               description
               (handler #f)
               (type 'boolean)
               (default #f)
             ) ;make-extension-flag-def
      (unless (and (string? flag) (> (string-length flag) 0))
        (type-error "make-extension-flag-def: flag must be a non-empty string")
      ) ;unless
      (unless (string? description)
        (type-error "make-extension-flag-def: description must be a string")
      ) ;unless
      (%make-extension-flag-def flag description handler type default)
    ) ;define*

    ;; 6. ExtensionCommandDef
    (define-record-type <extension-command-def>
      (%make-extension-command-def name description handler params)
      extension-command-def?
      (name extension-command-def-name)
      (description extension-command-def-description)
      (handler extension-command-def-handler)
      (params extension-command-def-params)
    ) ;define-record-type

    (define* (make-extension-command-def name description handler (params '()))
      (unless (and (string? name) (> (string-length name) 0))
        (type-error "make-extension-command-def: name must be a non-empty string")
      ) ;unless
      (unless (string? description)
        (type-error "make-extension-command-def: description must be a string")
      ) ;unless
      (unless (procedure? handler)
        (type-error "make-extension-command-def: handler must be a procedure")
      ) ;unless
      (%make-extension-command-def name description handler params)
    ) ;define*

  ) ;begin
) ;define-library
