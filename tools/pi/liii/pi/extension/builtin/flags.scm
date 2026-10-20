;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi extension builtin flags)
  (import (scheme base))
  (export register-builtin-flags!)
  (begin

    (define (register-builtin-flags! reg)
      reg
    ) ;define

  ) ;begin
) ;define-library
