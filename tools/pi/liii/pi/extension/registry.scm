;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi extension registry)
  (import (scheme base)
    (liii pi extension registry base)
    (liii pi extension registry tools)
    (liii pi extension registry flags)
    (liii pi extension registry commands)
    (liii pi extension registry flows)
  ) ;import
  (export target->registry
    upsert-by-key
    extension-registry-get-listeners
    current-extension-registry
    set-current-extension-registry!
    with-extension-registry
    default-extension-registry
    set-default-extension-registry!
    set-registry-initializer!
    add-registry-initializer!

    normalize-tool-def
    register-tool!
    extension-register-tool!
    find-extension-tool
    extension-registry-tools-list

    normalize-flag-def
    register-flag!
    extension-register-flag!
    find-extension-flag
    extension-registry-flags-list

    normalize-command-def
    register-command!
    extension-register-command!
    find-extension-command
    extension-registry-commands-list

    normalize-flow-def
    register-flow!
    extension-register-flow!
    find-extension-flow
    extension-registry-flows-list

    register-validator!
    extension-register-validator!
    extension-registry-validators-list

    register-config-augmenter!
    extension-registry-augmenters-list
  ) ;export
  (begin
  ) ;begin
) ;define-library
