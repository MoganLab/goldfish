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
  (display "=== define-record-type Tests ===")
  (newline)
  (newline)

  ;; Define record types with different field counts
  (define-record-type :point
    (make-point x y)
    point?
    (x point-x)
    (y point-y))

  (define-record-type :person
    (make-person name age city job)
    person?
    (name person-name)
    (age person-age)
    (city person-city)
    (job person-job))

  (define-record-type :big-record
    (make-big-record f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 f15 f16)
    big-record?
    (f1 big-record-f1)
    (f2 big-record-f2)
    (f3 big-record-f3)
    (f4 big-record-f4)
    (f5 big-record-f5)
    (f6 big-record-f6)
    (f7 big-record-f7)
    (f8 big-record-f8)
    (f9 big-record-f9)
    (f10 big-record-f10)
    (f11 big-record-f11)
    (f12 big-record-f12)
    (f13 big-record-f13)
    (f14 big-record-f14)
    (f15 big-record-f15)
    (f16 big-record-f16))

  ;; 14. Create simple record (2 fields + type tag = 4 inlet args)
  (bench-case "create record 2 fields"
    (lambda () (make-point 1 2))
    2000000)

  ;; 15. Create medium record (4 fields + type tag = 8 inlet args)
  (bench-case "create record 4 fields"
    (lambda () (make-person "Alice" 30 "NYC" "Dev"))
    1000000)

  ;; 16. Create big record (16 fields + type tag = 32 inlet args)
  (bench-case "create record 16 fields"
    (lambda () (make-big-record 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16))
    500000)

  ;; 17. Access record field (let-ref underlying)
  (define test-person (make-person "Alice" 30 "NYC" "Dev"))
  (bench-case "access record field (let-ref)"
    (lambda () (person-name test-person))
    20000000)

  ;; 18. Record type predicate
  (bench-case "record type predicate"
    (lambda () (person? test-person))
    20000000)

  ;; 19. Compare: direct inlet vs record create (2 fields)
  (bench-case "direct inlet 4 args (equiv to record 2f)"
    (lambda () (inlet 'type 'point 'x 1 'y 2))
    2000000)

  ;; 20. Compare: direct inlet vs record create (4 fields)
  (bench-case "direct inlet 8 args (equiv to record 4f)"
    (lambda () (inlet 'type 'person 'name "Alice" 'age 30 'city "NYC" 'job "Dev"))
    1000000)

  (newline)
  (display "=== Benchmark completed ===")
  (newline))

(run-all-tests)
