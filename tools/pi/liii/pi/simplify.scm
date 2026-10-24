;;
;; Ported from pi-simplify (https://github.com/MattDevy/pi-extensions)
;; Original work Copyright (c) 2026 Matt Devy
;; Goldfish Scheme port Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi simplify)
  (import (liii pi simplify ranges)
    (liii pi simplify scope)
    (liii pi simplify prompt)
    (liii pi simplify parse)
    (liii pi simplify format)
    (liii pi simplify modes)
  ) ;import
  (export pi-parse-changed-ranges pi-diff-ranges-command
    pi-collect-simplify-files pi-build-scope-lines pi-format-range
    pi-build-simplify-prompt pi-build-simplify-apply-prompt
    pi-parse-simplify-suggestions pi-format-simplify-suggestions
    run-simplify-flow
  ) ;export
  (begin
  ) ;begin
) ;define-library
