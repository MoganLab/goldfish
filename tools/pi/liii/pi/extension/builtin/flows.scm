;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi extension builtin flows)
  (import (scheme base)
    (liii pi extension types)
    (liii pi extension registry)
    (liii pi blueprint args)
    (liii pi compass args)
    (liii pi modes blueprint)
    (liii pi modes compass)
  ) ;import
  (export register-builtin-flows!)
  (begin

    (define (%ref parsed key)
      (let ((p (assoc key parsed)))
        (if p (cdr p) #f)
      ) ;let
    ) ;define

    (define (%register-blueprint-flows! reg)
      (register-flow! reg
        (make-extension-flow-def 'blueprint-local
          pi-blueprint-local-flow?
          (lambda (parsed cfg mode initial-msg)
            (run-blueprint-flow parsed cfg mode initial-msg)
          ) ;lambda
          :local?   #t
          :priority 20
        ) ;make-extension-flow-def
      ) ;register-flow!
      (register-flow! reg
        (make-extension-flow-def 'blueprint-session
          pi-blueprint-flag?
          (lambda (parsed cfg mode initial-msg)
            (run-blueprint-flow parsed cfg mode initial-msg)
          ) ;lambda
          :local?   #f
          :priority 20
        ) ;make-extension-flow-def
      ) ;register-flow!
    ) ;define

    (define (%register-compass-flows! reg)
      (register-config-augmenter! reg
        (lambda (cfg parsed) (pi-compass-augment-config parsed cfg))
      ) ;register-config-augmenter!
      (register-flow! reg
        (make-extension-flow-def 'compass-local
          pi-compass-local-flow?
          (lambda (parsed cfg mode initial-msg) (run-compass-flow parsed mode))
          :local?   #t
          :priority 30
        ) ;make-extension-flow-def
      ) ;register-flow!
    ) ;define

    (define (register-builtin-flows! reg)
      (%register-blueprint-flows! reg)
      (%register-compass-flows! reg)
      reg
    ) ;define

  ) ;begin
) ;define-library
