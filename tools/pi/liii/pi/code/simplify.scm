;;
;; Ported from pi-simplify (https://github.com/MattDevy/pi-extensions)
;; Original work Copyright (c) 2026 Matt Devy
;; Goldfish Scheme port Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code simplify)
  (import (liii pi code simplify ranges)
    (liii pi code simplify scope)
    (liii pi code simplify prompt)
    (liii pi code simplify parse)
    (liii pi code simplify format)
  ) ;import
  (export pi-parse-changed-ranges pi-diff-ranges-command
    pi-collect-simplify-files pi-build-scope-lines pi-format-range
    pi-build-simplify-prompt pi-build-simplify-apply-prompt
    pi-parse-simplify-suggestions pi-format-simplify-suggestions
  ) ;export
  (begin
  ) ;begin
) ;define-library
