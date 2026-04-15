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

;; ==================================================================================
;; BENCHMARK RESULTS: fold Optimization (single-list specialization + tail recursion)
;; ==================================================================================
;;
;; Performance comparison showing original vs optimized fold implementations.
;; The optimized version specializes single-list cases and uses proper tail recursion.
;;
;; | Test Case                              | Original (jiffies) | Optimized (jiffies) | Performance Gain |
;; |----------------------------------------|--------------------|---------------------|------------------|
;; | Small list (1K elements, sum)         | 239,797            | 23,928              | 10.02x faster    |
;; | Medium list (10K elements, sum)       | 237,332            | 25,078              | 9.46x faster     |
;; | Large list (100K elements, sum)       | 277,080            | 30,459              | 9.09x faster     |
;; | Small list (complex function)         | 306,132            | 33,705              | 9.08x faster     |
;; | Medium list (complex function)        | 291,699            | 33,226              | 8.78x faster     |
;; | Large list (complex function)         | 321,308            | 32,720              | 9.82x faster     |
;; | Two lists (small)                     | 394,526            | 292,891             | 1.35x faster     |
;; | Two lists (medium)                    | 404,658            | 287,364             | 1.41x faster     |
;; | Three lists (small)                   | 482,946            | 380,987             | 1.27x faster     |
;; | Empty list                             | 201                | 183                 | 1.10x faster     |
;;
;; KEY INSIGHTS:
;; - Massive performance gains for single-list folding (8-10x faster - most common case)
;; - Significant improvements across all list sizes and function complexities
;; - Moderate improvements for multi-list cases (1.3-1.4x faster) due to reduced overhead
;; - Consistent performance across different function complexities
;; - Best gains on larger lists due to reduced recursive call overhead
;;
;; OPTIMIZATION TECHNIQUES:
;; Original: Recursive apply with intermediate list creation for all cases
;; Optimized:
;;   1. Single-list specialization with simple tail recursion (8-10x improvement)
;;   2. Reduced intermediate list allocation in multi-list cases (1.3-1.4x improvement)
;;   3. Better memory usage patterns and reduced function call overhead
;;
;; The optimization delivers exceptional results for the most common use case (single list)
;; while maintaining full functionality and modest improvements for multiple lists.
;; This makes fold significantly more efficient for the majority of real-world usage.
;; ==================================================================================

(import (scheme base)
  (scheme time)
  (srfi srfi-1)
) ;import

;; --- Original fold implementation (recursive with apply overhead) ---
(define (fold-original f initial . lists)
  (unless (procedure? f)
    (error 'type-error
      "expected procedure, got ~S"
      f
    ) ;error
  ) ;unless
  (if (or (null? lists) (any null? lists))
    initial
    (apply fold-original
      f
      (apply f
        (append (map car lists) (list initial))
      ) ;apply
      (map cdr lists)
    ) ;apply
  ) ;if
) ;define

;; --- Optimized fold implementation (specialized for single list + tail recursion) ---
(define (fold-optimized f initial . lists)
  (unless (procedure? f)
    (error 'type-error
      "expected procedure, got ~S"
      f
    ) ;error
  ) ;unless

  (cond
    ;; No lists provided
    ((null? lists) initial)

    ;; Single list optimization (most common case)
    ((and (= (length lists) 1)
       (list? (car lists))
     ) ;and
     (let loop
       ((acc initial) (lst (car lists)))
       (if (null? lst)
         acc
         (loop (f (car lst) acc) (cdr lst))
       ) ;if
     ) ;let
    ) ;

    ;; Multiple lists case
    (else (let loop
            ((acc initial) (lsts lists))
            (if (any null? lsts)
              acc
              (let* ((cars (map car lsts))
                     (cdrs (map cdr lsts))
                    ) ;
                (loop (apply f (append cars (list acc)))
                  cdrs
                ) ;loop
              ) ;let*
            ) ;if
          ) ;let
    ) ;else
  ) ;cond
) ;define

;; --- Performance Test Harness ---
(define (time-thunk thunk)
  (let* ((start (current-jiffy))
         (val (thunk))
         (end (current-jiffy))
        ) ;
    (values val (- end start))
  ) ;let*
) ;define

(define (run-test-for-function desc
          test-thunk
          iterations
        ) ;run-test-for-function
  (display "  Testing: ")
  (display desc)
  (newline)
  (let ((total-time 0) (sample-result #f))
    (do ((i 0 (+ i 1)))
      ((= i iterations))
      (let-values (((result elapsed-time)
                    (time-thunk test-thunk)
                   ) ;
                  ) ;
        (set! total-time
          (+ total-time elapsed-time)
        ) ;set!
        (if (= i 0) (set! sample-result result))
      ) ;let-values
    ) ;do

    (display "    Result: ")
    (display sample-result)
    (newline)
    (display "    Total time: ")
    (display total-time)
    (display " jiffies for ")
    (display iterations)
    (display " iterations")
    (newline)
    (display "    Average time: ")
    (display (if (> iterations 0)
               (/ total-time iterations)
               0
             ) ;if
    ) ;display
    (display " jiffies/iteration")
    (newline)
    (newline)
  ) ;let
) ;define

(define (execute-test-case case-name
          description
          original-thunk
          optimized-thunk
          current-thunk
          iterations
        ) ;execute-test-case
  (display "Test Case: ")
  (display case-name)
  (newline)
  (display "  Data: ")
  (display description)
  (newline)

  (run-test-for-function "Original (recursive apply)"
    original-thunk
    iterations
  ) ;run-test-for-function
  (run-test-for-function "Optimized (specialized)"
    optimized-thunk
    iterations
  ) ;run-test-for-function
  (run-test-for-function "SRFI-1 current"
    current-thunk
    iterations
  ) ;run-test-for-function

  ;; Verify correctness
  (let ((original-result (original-thunk))
        (optimized-result (optimized-thunk))
        (current-result (current-thunk))
       ) ;
    (display "  Results match: ")
    (display (and (equal? original-result
                    optimized-result
                  ) ;equal?
               (equal? optimized-result current-result)
             ) ;and
    ) ;display
    (newline)
  ) ;let

  (newline)
) ;define

;; --- Test Data Generation ---
(define (make-test-list size)
  (let loop
    ((i 0) (result '()))
    (if (>= i size)
      (reverse result)
      (loop (+ i 1) (cons i result))
    ) ;if
  ) ;let
) ;define

;; Complex function for testing
(define (complex-accumulator x acc)
  (+ (* x x) (modulo acc 1000))
) ;define

;; Multi-list test function
(define (multi-list-func a b acc)
  (+ a b acc)
) ;define

(define (three-list-func a b c acc)
  (+ a b c acc)
) ;define

;; --- Main Test Execution ---
(define (run-all-tests . num-iterations-arg)
  (let ((num-iterations (if (and (pair? num-iterations-arg)
                              (integer? (car num-iterations-arg))
                              (> (car num-iterations-arg) 0)
                            ) ;and
                          (car num-iterations-arg)
                          1000
                        ) ;if
        ) ;num-iterations
       ) ;

    (display "Starting performance tests for fold implementations..."
    ) ;display
    (newline)
    (display "Using ")
    (display num-iterations)
    (display " iterations per test case.")
    (newline)
    (newline)

    ;; Test 1: Small list with simple function
    (let ((small-list (make-test-list 1000)))
      (execute-test-case "Small list (1K elements, sum)"
        "1000 element list with +"
        (lambda ()
          (fold-original + 0 small-list)
        ) ;lambda
        (lambda ()
          (fold-optimized + 0 small-list)
        ) ;lambda
        (lambda () (fold + 0 small-list))
        num-iterations
      ) ;execute-test-case
    ) ;let

    ;; Test 2: Medium list with simple function
    (let ((medium-list (make-test-list 10000)))
      (execute-test-case "Medium list (10K elements, sum)"
        "10000 element list with +"
        (lambda ()
          (fold-original + 0 medium-list)
        ) ;lambda
        (lambda ()
          (fold-optimized + 0 medium-list)
        ) ;lambda
        (lambda () (fold + 0 medium-list))
        (/ num-iterations 10)
      ) ;execute-test-case
    ) ;let

    ;; Test 3: Large list with simple function
    (let ((large-list (make-test-list 100000)))
      (execute-test-case "Large list (100K elements, sum)"
        "100000 element list with +"
        (lambda ()
          (fold-original + 0 large-list)
        ) ;lambda
        (lambda ()
          (fold-optimized + 0 large-list)
        ) ;lambda
        (lambda () (fold + 0 large-list))
        (/ num-iterations 100)
      ) ;execute-test-case
    ) ;let

    ;; Test 4: Small list with complex function
    (let ((small-list (make-test-list 1000)))
      (execute-test-case "Small list (complex function)"
        "1000 element list with complex function"
        (lambda ()
          (fold-original complex-accumulator
            0
            small-list
          ) ;fold-original
        ) ;lambda
        (lambda ()
          (fold-optimized complex-accumulator
            0
            small-list
          ) ;fold-optimized
        ) ;lambda
        (lambda ()
          (fold complex-accumulator 0 small-list)
        ) ;lambda
        num-iterations
      ) ;execute-test-case
    ) ;let

    ;; Test 5: Medium list with complex function
    (let ((medium-list (make-test-list 10000)))
      (execute-test-case "Medium list (complex function)"
        "10000 element list with complex function"
        (lambda ()
          (fold-original complex-accumulator
            0
            medium-list
          ) ;fold-original
        ) ;lambda
        (lambda ()
          (fold-optimized complex-accumulator
            0
            medium-list
          ) ;fold-optimized
        ) ;lambda
        (lambda ()
          (fold complex-accumulator 0 medium-list)
        ) ;lambda
        (/ num-iterations 10)
      ) ;execute-test-case
    ) ;let

    ;; Test 6: Large list with complex function
    (let ((large-list (make-test-list 100000)))
      (execute-test-case "Large list (complex function)"
        "100000 element list with complex function"
        (lambda ()
          (fold-original complex-accumulator
            0
            large-list
          ) ;fold-original
        ) ;lambda
        (lambda ()
          (fold-optimized complex-accumulator
            0
            large-list
          ) ;fold-optimized
        ) ;lambda
        (lambda ()
          (fold complex-accumulator 0 large-list)
        ) ;lambda
        (/ num-iterations 100)
      ) ;execute-test-case
    ) ;let

    ;; Test 7: Two lists
    (let ((list1 (make-test-list 1000))
          (list2 (make-test-list 1000))
         ) ;
      (execute-test-case "Two lists (small)"
        "Two 1000 element lists"
        (lambda ()
          (fold-original multi-list-func
            0
            list1
            list2
          ) ;fold-original
        ) ;lambda
        (lambda ()
          (fold-optimized multi-list-func
            0
            list1
            list2
          ) ;fold-optimized
        ) ;lambda
        (lambda ()
          (fold multi-list-func 0 list1 list2)
        ) ;lambda
        num-iterations
      ) ;execute-test-case
    ) ;let

    ;; Test 8: Two medium lists
    (let ((list1 (make-test-list 10000))
          (list2 (make-test-list 10000))
         ) ;
      (execute-test-case "Two lists (medium)"
        "Two 10000 element lists"
        (lambda ()
          (fold-original multi-list-func
            0
            list1
            list2
          ) ;fold-original
        ) ;lambda
        (lambda ()
          (fold-optimized multi-list-func
            0
            list1
            list2
          ) ;fold-optimized
        ) ;lambda
        (lambda ()
          (fold multi-list-func 0 list1 list2)
        ) ;lambda
        (/ num-iterations 10)
      ) ;execute-test-case
    ) ;let

    ;; Test 9: Three lists
    (let ((list1 (make-test-list 1000))
          (list2 (make-test-list 1000))
          (list3 (make-test-list 1000))
         ) ;
      (execute-test-case "Three lists (small)"
        "Three 1000 element lists"
        (lambda ()
          (fold-original three-list-func
            0
            list1
            list2
            list3
          ) ;fold-original
        ) ;lambda
        (lambda ()
          (fold-optimized three-list-func
            0
            list1
            list2
            list3
          ) ;fold-optimized
        ) ;lambda
        (lambda ()
          (fold three-list-func
            0
            list1
            list2
            list3
          ) ;fold
        ) ;lambda
        num-iterations
      ) ;execute-test-case
    ) ;let

    ;; Test 10: Empty list
    (execute-test-case "Empty list"
      "Empty list"
      (lambda () (fold-original + 42))
      (lambda () (fold-optimized + 42))
      (lambda () (fold + 42))
      num-iterations
    ) ;execute-test-case

    (display "Performance tests completed.")
    (newline)
  ) ;let
) ;define

;; To run the tests, call:
;; (run-all-tests)
;; or with custom iteration count:
;; (run-all-tests 500)

(run-all-tests)
