;;
;; Ported from pi-compass (https://github.com/MattDevy/pi-extensions)
;; Original work Copyright (c) 2026 Matt Devy
;; Goldfish Scheme port Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi compass)
  (import (scheme base)
    (liii pi compass types)
    (liii pi compass project)
    (liii pi compass tree)
    (liii pi compass packages)
    (liii pi compass frameworks)
    (liii pi compass entry)
    (liii pi compass build)
    (liii pi compass conventions)
    (liii pi compass keyfiles)
    (liii pi compass generate)
    (liii pi compass render)
    (liii pi compass store)
    (liii pi compass tour)
    (liii pi compass prompt)
    (liii pi compass tools)
    (liii pi compass args)
  ) ;import
  (export pi-compass-null? pi-compass-object? pi-compass-jref pi-compass->list
    pi-compass-directory-entry pi-compass-entry-name pi-compass-entry-type
    pi-compass-entry-children pi-compass-entry-dir? pi-compass-package
    pi-compass-package-manager pi-compass-package-name
    pi-compass-package-version pi-compass-package-dependencies
    pi-compass-framework pi-compass-framework-name
    pi-compass-framework-confidence pi-compass-framework-source
    pi-compass-entry-point pi-compass-entry-point-path
    pi-compass-entry-point-kind pi-compass-build-script
    pi-compass-build-script-name pi-compass-build-script-command
    pi-compass-build-script-source pi-compass-convention
    pi-compass-convention-source pi-compass-convention-content
    pi-compass-key-file pi-compass-key-file-path pi-compass-key-file-description
    pi-compass-codemap pi-compass-codemap? pi-compass-codemap-project-id
    pi-compass-codemap-project-name pi-compass-codemap-generated-at
    pi-compass-codemap-content-hash pi-compass-codemap-tree
    pi-compass-codemap-packages pi-compass-codemap-frameworks
    pi-compass-codemap-entry-points pi-compass-codemap-build-scripts
    pi-compass-codemap-conventions pi-compass-codemap-key-files
    pi-compass-tour-step pi-compass-tour pi-compass-tour-project-id
    pi-compass-tour-topic pi-compass-tour-generated-at pi-compass-tour-steps
    pi-compass-step-file pi-compass-step-description pi-compass-hash-id
    pi-compass-detect-project pi-compass-project-id pi-compass-project-name
    pi-compass-project-root pi-compass-project-remote
    pi-compass-build-directory-tree pi-compass-format-directory-tree
    pi-compass-read-text pi-compass-read-json pi-compass-object-keys
    pi-compass-detect-packages pi-compass-detect-frameworks
    pi-compass-detect-entry-points pi-compass-detect-build-scripts
    pi-compass-detect-conventions pi-compass-truncate-convention
    pi-compass-detect-key-files pi-compass-content-hash
    pi-compass-generate-codemap pi-compass-render-codemap
    pi-compass-truncate-codemap pi-compass-default-codemap-path
    pi-compass-resolve-path pi-compass-load-codemap pi-compass-save-codemap!
    pi-compass-cache-entry pi-compass-get-or-generate pi-compass-cache-hit?
    pi-compass-detect-topics pi-compass-build-tour pi-compass-render-tour
    pi-compass-describe-file pi-compass-build-injection
    pi-compass-injection-text pi-compass-turn-system-prompt
    pi-codebase-map-schema pi-code-tour-schema pi-execute-codebase-map
    pi-execute-code-tour pi-codebase-map-definition pi-code-tour-definition
    pi-compass-extend-tool-names pi-compass-args-defaults pi-compass-handle-flag
    pi-compass-validate pi-compass-local-flow? pi-compass-flag?
    pi-compass-tour-topic-arg PI_COMPASS_HASH_FILES
    PI_COMPASS_MAX_INJECTION_CHARS
  ) ;export
  (begin
  ) ;begin
) ;define-library
