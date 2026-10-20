;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi extension builtin)
  (import (scheme base)
    (liii pi extension builtin flags)
    (liii pi extension builtin flows)
  ) ;import
  (export register-builtin-extensions!
    register-builtin-flags!
    register-builtin-flows!
  ) ;export
  (begin

    (define (register-builtin-extensions! reg)
      (register-builtin-flags! reg)
      (register-builtin-flows! reg)
      reg
    ) ;define

  ) ;begin
) ;define-library
