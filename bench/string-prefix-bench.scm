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
;; BENCHMARK RESULTS: string-prefix? Optimization (using substring)
;; ==================================================================================
;;
;; Performance comparison showing original vs optimized string-prefix? implementations.
;; The optimized version uses substring + string=? instead of character-by-character comparison.
;;
;; | Test Case                              | Original (jiffies) | Optimized (jiffies) | Performance Gain |
;; |----------------------------------------|--------------------|---------------------|------------------|
;; | Short prefix, short string (match)    | 19,005             | 14,087              | 1.35x faster     |
;; | Short prefix, short string (no match) | 17,843             | 14,047              | 1.27x faster     |
;; | Empty prefix                           | 14,252             | 13,956              | 1.02x faster     |
;; | Long prefix, long string (match)       | 215,960            | 15,573              | 13.87x faster    |
;; | Long string (no match - at start)     | 14,421             | 11,899              | 1.21x faster     |
;; | Long string (no match - at end)       | 197,421            | 25,612              | 7.71x faster     |
;; | Very long prefix (1000 chars)         | 181,494            | 5,613               | 32.34x faster    |
;; | Prefix same length as string          | 21,823             | 12,934              | 1.69x faster     |
;; | Prefix longer than string             | 13,090             | 13,370              | 0.98x (similar)  |
;;
;; KEY INSIGHTS:
;; - Massive performance gains for long prefix matching scenarios (13-32x faster)
;; - Consistent moderate improvements for short strings (1.2-1.7x faster)
;; - Best optimization occurs when the entire prefix needs to be compared
;; - Minimal overhead for edge cases (prefix longer than string)
;;
;; OPTIMIZATION TECHNIQUE:
;; Original: for i in 0..prefix_len: if prefix[i] != str[i] return false
;; Optimized: return prefix == substring(str, 0, prefix_len)
;;
;; The substring approach leverages optimized native string comparison instead of
;; character-by-character loops, resulting in significant performance improvements.
;; ==================================================================================

(import (scheme base)
  (scheme time)
  (srfi srfi-13)
) ;import

;; --- Original string-prefix? implementation (character-by-character comparison) ---
(define (string-prefix?-original prefix str)
  (let* ((prefix-len (string-length prefix))
         (str-len (string-length str))
        ) ;
    (and (<= prefix-len str-len)
      (let loop
        ((i 0))
        (or (= i prefix-len)
          (and (char=? (string-ref prefix i)
                 (string-ref str i)
               ) ;char=?
            (loop (+ i 1))
          ) ;and
        ) ;or
      ) ;let
    ) ;and
  ) ;let*
) ;define

;; --- Optimized string-prefix? implementation (using substring) ---
(define (string-prefix?-optimized prefix str)
  (let* ((prefix-len (string-length prefix))
         (str-len (string-length str))
        ) ;
    (and (<= prefix-len str-len)
      (string=? prefix
        (substring str 0 prefix-len)
      ) ;string=?
    ) ;and
  ) ;let*
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
          func
          prefix
          str
          num-iterations
        ) ;run-test-for-function
  (display "  Testing: ")
  (display desc)
  (newline)
  (let ((total-time 0) (sample-result #f))
    (do ((i 0 (+ i 1)))
      ((= i num-iterations))
      (let-values (((result elapsed-time)
                    (time-thunk (lambda () (func prefix str))
                    ) ;time-thunk
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
    (display num-iterations)
    (display " iterations")
    (newline)
    (display "    Average time: ")
    (display (if (> num-iterations 0)
               (/ total-time num-iterations)
               0
             ) ;if
    ) ;display
    (display " jiffies/iteration")
    (newline)
    (newline)
  ) ;let
) ;define

(define (execute-test-case case-name
          prefix
          str
          num-iterations
        ) ;execute-test-case
  (display "Test Case: ")
  (display case-name)
  (newline)
  (display "  Prefix: \"")
  (display prefix)
  (display "\"")
  (newline)
  (display "  String: \"")
  (display (if (> (string-length str) 50)
             (string-append (substring str 0 47)
               "..."
             ) ;string-append
             str
           ) ;if
  ) ;display
  (display "\" (length: ")
  (display (string-length str))
  (display ")")
  (newline)

  (run-test-for-function "Original (char-by-char)"
    string-prefix?-original
    prefix
    str
    num-iterations
  ) ;run-test-for-function
  (run-test-for-function "Optimized (substring)"
    string-prefix?-optimized
    prefix
    str
    num-iterations
  ) ;run-test-for-function
  (run-test-for-function "SRFI-13 current"
    string-prefix?
    prefix
    str
    num-iterations
  ) ;run-test-for-function

  (newline)
) ;define

;; --- Test Data Generation ---
(define (make-test-string length char)
  (make-string length char)
) ;define

(define (make-mixed-string length)
  (list->string (map (lambda (i)
                       (integer->char (+ (char->integer #\a) (modulo i 26))
                       ) ;integer->char
                     ) ;lambda
                  (iota length)
                ) ;map
  ) ;list->string
) ;define

;; Simple iota implementation if not available
(define (iota count . rest)
  (let ((start (if (null? rest) 0 (car rest)))
        (step (if (or (null? rest) (null? (cdr rest)))
                1
                (cadr rest)
              ) ;if
        ) ;step
       ) ;
    (let loop
      ((i 0) (result '()))
      (if (>= i count)
        (reverse result)
        (loop (+ i 1)
          (cons (+ start (* i step)) result)
        ) ;loop
      ) ;if
    ) ;let
  ) ;let
) ;define

;; --- Main Test Execution ---
(define (run-all-tests . num-iterations-arg)
  (let ((num-iterations (if (and (pair? num-iterations-arg)
                              (integer? (car num-iterations-arg))
                              (> (car num-iterations-arg) 0)
                            ) ;and
                          (car num-iterations-arg)
                          100000
                        ) ;if
        ) ;num-iterations
       ) ;

    (display "Starting performance tests for string-prefix? implementations..."
    ) ;display
    (newline)
    (display "Using ")
    (display num-iterations)
    (display " iterations per test case.")
    (newline)
    (newline)

    ;; Test 1: Short prefix, short string
    (execute-test-case "Short prefix, short string (match)"
      "he"
      "hello"
      num-iterations
    ) ;execute-test-case

    ;; Test 2: Short prefix, short string (no match)
    (execute-test-case "Short prefix, short string (no match)"
      "hi"
      "hello"
      num-iterations
    ) ;execute-test-case

    ;; Test 3: Empty prefix
    (execute-test-case "Empty prefix"
      ""
      "hello world"
      num-iterations
    ) ;execute-test-case

    ;; Test 4: Long prefix, long string (match)
    (let ((long-string (make-mixed-string 1000)))
      (execute-test-case "Long prefix, long string (match)"
        (substring long-string 0 100)
        long-string
        num-iterations
      ) ;execute-test-case
    ) ;let

    ;; Test 5: Long prefix, long string (no match - differs at start)
    (let ((long-string (make-mixed-string 1000)))
      (execute-test-case "Long prefix, long string (no match - differs at start)"
        "xyz"
        long-string
        num-iterations
      ) ;execute-test-case
    ) ;let

    ;; Test 6: Long prefix, long string (no match - differs at end)
    (let* ((long-string (make-mixed-string 1000))
           (almost-prefix (string-append (substring long-string 0 99)
                            "X"
                          ) ;string-append
           ) ;almost-prefix
          ) ;
      (execute-test-case "Long prefix, long string (no match - differs at end)"
        almost-prefix
        long-string
        num-iterations
      ) ;execute-test-case
    ) ;let*

    ;; Test 7: Very long strings
    (let ((very-long-string (make-mixed-string 10000)
          ) ;very-long-string
         ) ;
      (execute-test-case "Very long prefix, very long string (match)"
        (substring very-long-string 0 1000)
        very-long-string
        (/ num-iterations 10)
      ) ;execute-test-case
    ) ;let

    ;; Test 8: Pathological case - prefix same length as string
    (execute-test-case "Prefix same length as string (match)"
      "hello"
      "hello"
      num-iterations
    ) ;execute-test-case

    ;; Test 9: Prefix longer than string
    (execute-test-case "Prefix longer than string"
      "hello world"
      "hello"
      num-iterations
    ) ;execute-test-case

    (display "Performance tests completed.")
    (newline)
  ) ;let
) ;define

;; To run the tests, call:
;; (run-all-tests)
;; or with custom iteration count:
;; (run-all-tests 50000)

(run-all-tests)
