;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
;; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
;; License for the specific language governing permissions and limitations
;; under the License.
;;

(define-library (scheme eval)
  (import (scheme base) (goldfish))
  (export environment eval)
  (begin

    ;; Native eval, resolved at call time.  Keeping the host evaluator out of
    ;; the module's top-level values avoids placing an evaluator primitive in
    ;; the runtime module environment during native restoration.
    (define (%s7-eval expr . maybe-env)
      (let ((host-eval (symbol->value 'eval)))
        (if (pair? maybe-env)
          (host-eval expr (car maybe-env))
          (host-eval expr))))

    ;; R7RS (scheme eval): environment builds a program environment whose
    ;; bindings come from the given import-sets (only / except / prefix /
    ;; rename included), implemented by the expander's
    ;; make-program-environment; eval then expands the expression with the
    ;; Sets-of-Scopes expander so macros from the environment's libraries
    ;; (e.g. srfi-8's receive) work, instead of s7's macro-less native eval.

    (define (environment . import-sets)
      (make-program-environment import-sets))

    (define* (eval expr (env #f))
      (if env
        (if (eval-environment? env)
          (%s7-eval expr env)
          (eval-in-program-environment expr env))
        (%s7-eval expr)))

  ) ;begin
) ;define-library
