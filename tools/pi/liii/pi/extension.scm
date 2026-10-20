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
    set-extension-registry-tools!
    set-extension-registry-flags!
    set-extension-registry-commands!
    set-extension-registry-listeners!
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

    ;; ExtensionCommandDef
    make-extension-command-def
    extension-command-def?
    extension-command-def-name
    extension-command-def-description
    extension-command-def-handler
    extension-command-def-params

    ;; Registry Operations
    target->registry
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
