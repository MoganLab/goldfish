;; 0-clause BSD
;; Parameter objects: SRFI-39 compatibility.  (scheme base) already provides
;; make-parameter/parameterize (with the SRFI-39 optional converter), so this
;; library re-exports those bindings instead of defining its own -- a program
;; importing both gets the same binding, never a duplicate-name conflict.

(define-library (srfi srfi-39)
  (import (scheme base))
  (export make-parameter parameterize)
  (begin)
) ;define-library
