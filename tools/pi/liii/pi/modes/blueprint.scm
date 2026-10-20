;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi modes blueprint)
  (import (scheme base)
    (liii pi args)
    (liii pi modes blueprint common)
    (liii pi modes blueprint generate)
    (liii pi modes blueprint manage)
    (liii pi modes blueprint verify)
  ) ;import
  (export run-blueprint-flow pi-blueprint-extend-tool-names)
  (begin

    (define (pi-blueprint-extend-tool-names base parsed)
      (pi-bp-mode-extend-tools base parsed)
    ) ;define

    (define (run-blueprint-flow parsed cfg mode initial-msg)
      (cond ((pi-args-ref parsed 'blueprint)
             (run-blueprint-generate-flow parsed cfg mode initial-msg)
            ) ;
            ((pi-args-ref parsed 'plan-status) (run-blueprint-status-flow parsed mode))
            ((pi-args-ref parsed 'plan-next) (run-blueprint-next-flow parsed cfg mode))
            ((pi-args-ref parsed 'plan-verify) (run-blueprint-verify-flow parsed cfg mode))
            ((pi-args-ref parsed 'plan-abandon) (run-blueprint-abandon-flow parsed mode))
            (else 0)
      ) ;cond
    ) ;define

  ) ;begin
) ;define-library
