;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi extension builtin)
  (import (scheme base)
    (liii pi extension builtin flags)
    (liii pi extension builtin flows)
    (liii pi extension builtin tdd)
    (liii pi extension builtin prompts)
  ) ;import
  (export register-builtin-extensions! register-builtin-flags!
    register-builtin-flows! register-builtin-tdd-hooks!
    register-builtin-prompt-injectors!
  ) ;export
  (begin

    (define (register-builtin-extensions! reg)
      (register-builtin-flags! reg)
      (register-builtin-flows! reg)
      (register-builtin-tdd-hooks! reg)
      (register-builtin-prompt-injectors! reg)
      reg
    ) ;define

  ) ;begin
) ;define-library
