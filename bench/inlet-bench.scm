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

(import (scheme base)
  (scheme time)
  (liii timeit)
) ;import

(define big-inlet-size 6000)

;; Generate a big inlet with N symbol/value pairs using varlet
(define (make-big-inlet n)
  (let ((env (inlet)))
    (do ((i 0 (+ i 1)))
        ((>= i n) env)
      (varlet env (string->symbol (string-append "sym-" (number->string i))) i))))

;; --- Performance Test Harness ---
(define (bench-case label thunk iterations)
  (let ((t (timeit thunk '() iterations)))
    (display label)
    (display ": ")
    (display t)
    (display "s for ")
    (display iterations)
    (display " iterations")
    (newline)))

(define (run-all-tests)
  (display "=== Inlet Performance Benchmark ===")
  (newline)
  (display "Big inlet size: ")
  (display big-inlet-size)
  (newline)
  (newline)

  ;; 1. Empty inlet -- goes through s7_inlet -> sublet_1
  (bench-case "empty inlet"
    (lambda () (inlet))
    10000000)

  ;; 2. simple_inlet path -- 2 args
  (bench-case "simple inlet 2 args"
    (lambda () (inlet 'a 1))
    5000000)

  ;; 3. simple_inlet path -- 8 args
  (bench-case "simple inlet 8 args"
    (lambda () (inlet 'a 1 'b 2 'c 3 'd 4 'e 5 'f 6 'g 7 'h 8))
    1000000)

  ;; 4. generic path -- with cons args (goes through s7_inlet -> sublet_1)
  (bench-case "generic inlet (with cons)"
    (lambda () (inlet '(a . 1) '(b . 2)))
    2000000)

  ;; 5. generic path -- with let arg (goes through s7_inlet -> sublet_1)
  (let ((e (inlet 'x 1 'y 2)))
    (bench-case "generic inlet (with let)"
      (lambda () (inlet 'a 1 e))
      2000000))

  (newline)
  (display "=== Big Inlet Tests ===")
  (newline)
  (newline)

  ;; 6. Create big inlet incrementally via sublet
  (bench-case (string-append "create big inlet (" (number->string big-inlet-size) " via sublet loop)")
    (lambda () (make-big-inlet big-inlet-size))
    200)

  ;; Pre-create big inlet for subsequent tests
  (define big-inlet (make-big-inlet big-inlet-size))
  (display "Big inlet created, slot count: ")
  (display (length (let->list big-inlet)))
  (newline)
  (newline)

  ;; 7. Copy big inlet via inlet (triggers append_let in sublet_1)
  (bench-case "copy big inlet via (inlet big-inlet)"
    (lambda () (inlet big-inlet))
    1000)

  ;; 8. Copy big inlet via sublet (no actual copy, just makes empty child let)
  (bench-case "copy big inlet via (sublet big-inlet)"
    (lambda () (sublet big-inlet))
    1000000)

  ;; 9. Merge big inlet with small inlet
  (bench-case "merge big inlet + small inlet"
    (lambda () (inlet 'a 1 big-inlet))
    1000)

  (newline)
  (display "=== Big Inlet Symbol Lookup Tests ===")
  (newline)
  (newline)

  ;; 10. defined? on existing symbol in big inlet
  (bench-case "defined? existing symbol in big inlet"
    (lambda () (defined? 'sym-100 big-inlet))
    50000)

  ;; 11. defined? on non-existing symbol in big inlet
  (bench-case "defined? non-existing symbol in big inlet"
    (lambda () (defined? 'nonexistent big-inlet))
    10000000)

  ;; 12. let-ref on existing symbol in big inlet
  (bench-case "let-ref existing symbol in big inlet"
    (lambda () (let-ref big-inlet 'sym-100))
    50000)

  ;; 13. let-ref on last symbol in big inlet
  (let ((last-sym (string->symbol (string-append "sym-" (number->string (- big-inlet-size 1))))))
    (bench-case "let-ref last symbol in big inlet"
      (lambda () (let-ref big-inlet last-sym))
      10000000))

  (newline)
  (display "=== Benchmark completed ===")
  (newline))

(run-all-tests)
