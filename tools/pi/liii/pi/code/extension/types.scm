;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code extension types)
  (import (scheme base)
    (liii pi code extension types core)
    (liii pi code extension types defs)
  ) ;import
  (export
    ;; ExtensionRegistry
    make-extension-registry
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
    extension-tool-def-default-active?

    ;; ExtensionFlagDef
    make-extension-flag-def
    extension-flag-def?
    extension-flag-def-flag
    extension-flag-def-description
    extension-flag-def-handler
    extension-flag-def-type
    extension-flag-def-default
    extension-flag-def-key
    extension-flag-def-aliases
    extension-flag-def-takes-arg?
    extension-flag-def-validator

    ;; ExtensionCommandDef
    make-extension-command-def
    extension-command-def?
    extension-command-def-name
    extension-command-def-description
    extension-command-def-handler
    extension-command-def-params

    ;; ExtensionFlowDef
    make-extension-flow-def
    extension-flow-def?
    extension-flow-def-name
    extension-flow-def-predicate
    extension-flow-def-handler
    extension-flow-def-local?
    extension-flow-def-priority
  ) ;export
  (begin
  ) ;begin
) ;define-library
