;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi extension)
  (import (scheme base)
    (liii pi extension types)
    (liii pi extension registry)
    (liii pi extension event)
    (liii pi extension discovery)
    (liii pi extension loader)
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
    set-extension-registry-tools!
    set-extension-registry-flags!
    set-extension-registry-commands!
    set-extension-registry-listeners!
    set-extension-registry-flows!
    set-extension-registry-validators!
    set-extension-registry-augmenters!
    extension-registry-get-listeners

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

    ;; Registry Operations
    target->registry
    default-extension-registry
    set-default-extension-registry!

    register-tool!
    extension-register-tool!
    find-extension-tool
    extension-registry-tools-list

    register-flag!
    extension-register-flag!
    find-extension-flag
    extension-registry-flags-list

    register-command!
    extension-register-command!
    find-extension-command
    extension-registry-commands-list

    register-flow!
    extension-register-flow!
    find-extension-flow
    extension-registry-flows-list

    register-validator!
    extension-register-validator!
    extension-registry-validators-list

    register-config-augmenter!
    extension-registry-augmenters-list

    current-extension-registry
    set-current-extension-registry!
    with-extension-registry

    ;; Event Lifecycle
    pi-on!
    pi-emit!

    ;; Discovery & Loader
    pi-scan-extension-dir
    pi-discover-extensions
    pi-default-builtin-extensions-dir
    pi-load-extension
    pi-load-extensions
    pi-load-results-success-count
    pi-load-results-failed-count
  ) ;export
  (begin
  ) ;begin
) ;define-library
