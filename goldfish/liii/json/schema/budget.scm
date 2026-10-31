;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;  http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;

(define-library (liii json schema budget)
  (import (scheme base) (liii json schema data))
  (export make-resource-budget resource-budget? reset-resource-budget!
    enter-budget-depth! leave-budget-depth! count-budget-step!
    count-budget-reference! budget-depth budget-steps budget-references
    budget-max-depth budget-max-steps budget-max-references
    validate-budget-options! resource-budget-from-options
  ) ;export
  (begin
    ;; A resource budget bounds the work a single validation may perform.
    ;;
    ;; The budget is a vector with six slots:
    ;;   0 current evaluation depth
    ;;   1 number of evaluated schema nodes
    ;;   2 number of resolved references
    ;;   3 maximum evaluation depth
    ;;   4 maximum number of evaluated schema nodes
    ;;   5 maximum number of resolved references
    ;;
    ;; The counters are reset before every validation run, so a validator
    ;; can be reused without the budget of one run leaking into the next.

    (define (make-resource-budget max-depth max-steps max-references)
      (vector 0 0 0 max-depth max-steps max-references)
    ) ;define

    (define (resource-budget? value)
      (and (vector? value) (= (vector-length value) 6))
    ) ;define

    (define (budget-depth budget)
      (vector-ref budget 0)
    ) ;define
    (define (budget-steps budget)
      (vector-ref budget 1)
    ) ;define
    (define (budget-references budget)
      (vector-ref budget 2)
    ) ;define
    (define (budget-max-depth budget)
      (vector-ref budget 3)
    ) ;define
    (define (budget-max-steps budget)
      (vector-ref budget 4)
    ) ;define
    (define (budget-max-references budget)
      (vector-ref budget 5)
    ) ;define

    (define (reset-resource-budget! budget)
      (vector-set! budget 0 0)
      (vector-set! budget 1 0)
      (vector-set! budget 2 0)
      budget
    ) ;define

    ;; Exceeding a budget is a hard failure with its own error type, so
    ;; that a caller can tell it apart from an invalid schema or an
    ;; unresolvable reference.
    (define (budget-exceeded! what limit)
      (error 'json-schema-budget-exceeded
        (string-append "JSON Schema resource budget exceeded: " what)
        limit
      ) ;error
    ) ;define

    (define (enter-budget-depth! budget)
      (let ((depth (+ (vector-ref budget 0) 1)))
        (vector-set! budget 0 depth)
        (when (> depth (vector-ref budget 3))
          (budget-exceeded! "maximum evaluation depth" (vector-ref budget 3))
        ) ;when
        depth
      ) ;let
    ) ;define

    (define (leave-budget-depth! budget)
      (let ((depth (- (vector-ref budget 0) 1)))
        (vector-set! budget 0 (if (< depth 0) 0 depth))
        depth
      ) ;let
    ) ;define

    (define (count-budget-step! budget)
      (let ((steps (+ (vector-ref budget 1) 1)))
        (vector-set! budget 1 steps)
        (when (> steps (vector-ref budget 4))
          (budget-exceeded! "maximum evaluation steps" (vector-ref budget 4))
        ) ;when
        steps
      ) ;let
    ) ;define

    ;; Validates the max-depth, max-steps and max-references options.
    (define (validate-budget-options! options)
      (for-each
        (lambda (name)
          (let ((value (option-ref options name #f)))
            (unless (or (not value) (and (integer? value) (exact? value) (> value 0)))
              (error 'json-schema-options-error
                (string-append (symbol->string name) " must be #f or a positive integer")
                value
              ) ;error
            ) ;unless
          ) ;let
        ) ;lambda
        '(max-depth max-steps max-references)
      ) ;for-each
    ) ;define
    ;; The default limits are generous enough for real documents and
    ;; small enough to stop runaway recursion in finite time.
    (define (resource-budget-from-options options)
      (make-resource-budget (option-ref options 'max-depth 1000)
        (option-ref options 'max-steps 1000000)
        (option-ref options 'max-references 100000)
      ) ;make-resource-budget
    ) ;define
    (define (count-budget-reference! budget)
      (let ((count (+ (vector-ref budget 2) 1)))
        (vector-set! budget 2 count)
        (when (> count (vector-ref budget 5))
          (budget-exceeded! "maximum reference resolutions" (vector-ref budget 5))
        ) ;when
        count
      ) ;let
    ) ;define
  ) ;begin
) ;define-library
