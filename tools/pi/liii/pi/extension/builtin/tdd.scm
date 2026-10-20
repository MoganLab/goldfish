;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi extension builtin tdd)
  (import (scheme base))
  (export register-builtin-tdd-hooks!)
  (begin

    (define (register-builtin-tdd-hooks! reg)
      reg
    ) ;define

  ) ;begin
) ;define-library
