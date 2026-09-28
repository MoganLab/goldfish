;; Native equivalent of the R7RS environment/eval contract.  Keep this
;; fixture independent of the host test runner's evaluation environment.
(import (scheme eval))

(define base-env (environment '(scheme base)))
(define renamed-env
  (environment '(rename (scheme base) (+ add))))

(if (not (vector? base-env))
    (error "native environment: base import"))
(if (not (vector? renamed-env))
    (error "native environment: rename import"))
(if (not (= (eval '(+ 20 22) base-env) 42))
    (error "native environment eval: base import"))
