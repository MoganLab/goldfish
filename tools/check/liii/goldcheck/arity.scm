;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
(define-library (liii goldcheck arity)
  (import (scheme base)
    (liii goldcheck config)
    (liii goldcheck path)
    (liii goldcheck arity internal scan)
  ) ;import
  (export check-arity %check-file-rule-arity)
  (begin
    (define (check-arity file)
      (%scan-file-arity file)
    ) ;define

    (define (%check-file-rule-arity file cfg acc)
      (if
        (and cfg
          (check-rule-enabled? cfg "arity")
          (not (file-excluded? file (check-rule-excludes cfg "arity")))
        ) ;and
        (append acc (check-arity file))
        acc
      ) ;if
    ) ;define
  ) ;begin
) ;define-library
