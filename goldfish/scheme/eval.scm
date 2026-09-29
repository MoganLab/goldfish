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

    ;; Native eval, resolved at call time from a private global name.
    ;; Looking up `eval' itself is unsafe: importing (scheme eval) can make
    ;; the visible `eval' this library's own wrapper, which then recurses
    ;; through %native-eval until the heap is exhausted. It is never
    ;; re-exported by user libraries.
    (define (%native-eval expr . maybe-env)
      (let ((native-eval (symbol->value '%native-eval)))
        (if (pair? maybe-env)
          (native-eval expr (car maybe-env))
          (native-eval expr))))

    ;; R7RS (scheme eval): environment builds a program environment whose
    ;; bindings come from the given import-sets (only / except / prefix /
    ;; rename included), implemented by the expander's
    ;; make-program-environment; eval then expands with the Sets-of-Scopes
    ;; expander so macros from the imported libraries work.

    (define (environment . import-sets)
      (make-program-environment import-sets))

    (define* (eval expr (env #f))
      (if env
        (if (and (defined? 'eval-environment?)
                 (eval-environment? env))
          (%native-eval expr env)
          (eval-in-program-environment expr env))
        (%native-eval expr)))

  ) ;begin
) ;define-library
