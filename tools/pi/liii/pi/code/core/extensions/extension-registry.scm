;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code core extensions extension-registry)
  (import (scheme base) (liii base))
  (export make-extension-registry extension-registry? extension-registry-tools
    extension-registry-flags extension-registry-commands
    extension-registry-listeners extension-registry-flows
    extension-registry-validators extension-registry-augmenters
    extension-registry-prompt-injectors set-extension-registry-tools!
    set-extension-registry-flags! set-extension-registry-commands!
    set-extension-registry-listeners! set-extension-registry-flows!
    set-extension-registry-validators! set-extension-registry-augmenters!
    set-extension-registry-prompt-injectors!
  ) ;export
  (begin

    (define*-record-type <extension-registry>
      (make-extension-registry (tools '())
        (flags '())
        (commands '())
        (listeners '())
        (flows '())
        (validators '())
        (augmenters '())
        (prompt-injectors '())
      ) ;make-extension-registry
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
    ) ;define*-record-type

  ) ;begin
) ;define-library
