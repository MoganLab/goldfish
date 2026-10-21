;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi extension builtin prompts)
  (import (scheme base))
  (export register-builtin-prompt-injectors!)
  (begin

    (define (register-builtin-prompt-injectors! reg)
      reg
    ) ;define

  ) ;begin
) ;define-library
