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
;; BENCHMARK RESULTS: string-suffix? Optimization (using substring)
;; ==================================================================================
;;
;; Performance comparison showing original vs optimized string-suffix? implementations.
;; The optimized version uses substring + string=? instead of character-by-character comparison.
;;
;; | Test Case                              | Original (jiffies) | Optimized (jiffies) | Performance Gain |
;; |----------------------------------------|--------------------|---------------------|------------------|
;; | Short suffix, short string (match)    | 28,494             | 17,877              | 1.59x faster     |
;; | Short suffix, short string (no match) | 25,037             | 17,637              | 1.42x faster     |
;; | Empty suffix                           | 23,635             | 17,756              | 1.33x faster     |
;; | Long suffix, long string (match)       | 206,644            | 17,375              | 11.90x faster    |
;; | Long string (no match - at end)       | 28,181             | 16,311              | 1.73x faster     |
;; | Long string (no match - at start)     | 28,553             | 19,164              | 1.49x faster     |
;; | Very long suffix (1000 chars)         | 198,558            | 2,432               | 81.65x faster    |
;; | Suffix same length as string          | 33,930             | 20,323              | 1.67x faster     |
;; | Suffix longer than string             | 19,779             | 20,610              | 0.96x (similar)  |
;;
;; KEY INSIGHTS:
;; - Massive performance gains for long suffix matching scenarios (12-82x faster)
;; - Consistent moderate improvements for short strings (1.3-1.7x faster)  
;; - Best optimization occurs when the entire suffix needs to be compared
;; - Minimal overhead for edge cases (suffix longer than string)
;; - File extension and URL patterns show good 1.5-1.7x improvements
;;
;; OPTIMIZATION TECHNIQUE:
;; Original: for i in 0..suffix_len: if suffix[i] != str[str_len - suffix_len + i] return false
;; Optimized: return suffix == substring(str, str_len - suffix_len, str_len)
;;
;; The substring approach leverages optimized native string comparison instead of
;; character-by-character loops, similar to string-prefix? optimization.
;; Results show this optimization is highly effective, especially for longer suffixes.
;; ==================================================================================

(import (scheme base)
        (scheme time)
        (srfi srfi-13))

;; --- Original string-suffix? implementation (character-by-character comparison) ---
(define (string-suffix?-original suffix str)
  (let* ((suffix-len (string-length suffix))
         (str-len (string-length str)))
    (and (<= suffix-len str-len)
         (let loop ((i 0)
                    (j (- str-len suffix-len)))
           (or (= i suffix-len)
               (and (char=? (string-ref suffix i)
                            (string-ref str j))
                    (loop (+ i 1) (+ j 1))))))))

;; --- Optimized string-suffix? implementation (using substring) ---
(define (string-suffix?-optimized suffix str)
  (let* ((suffix-len (string-length suffix))
         (str-len (string-length str)))
    (and (<= suffix-len str-len)
         (string=? suffix (substring str (- str-len suffix-len) str-len)))))

;; --- Performance Test Harness ---
(define (time-thunk thunk)
  (let* ((start (current-jiffy))
         (val (thunk))
         (end (current-jiffy)))
    (values val (- end start))))

(define (run-test-for-function desc func suffix str num-iterations)
  (display "  Testing: ") (display desc) (newline)
  (let ((total-time 0)
        (sample-result #f))
    (do ((i 0 (+ i 1)))
        ((= i num-iterations))
      (let-values (((result elapsed-time) (time-thunk (lambda () (func suffix str)))))
        (set! total-time (+ total-time elapsed-time))
        (if (= i 0) (set! sample-result result))))
    
    (display "    Result: ") (display sample-result) (newline)
    (display "    Total time: ") (display total-time) (display " jiffies for ") (display num-iterations) (display " iterations") (newline)
    (display "    Average time: ") (display (if (> num-iterations 0) (/ total-time num-iterations) 0)) (display " jiffies/iteration") (newline)
    (newline)))

(define (execute-test-case case-name suffix str num-iterations)
  (display "Test Case: ") (display case-name) (newline)
  (display "  Suffix: \"") (display suffix) (display "\"") (newline)
  (display "  String: \"") (display (if (> (string-length str) 50) 
                                        (string-append "..." (substring str (max 0 (- (string-length str) 47)) (string-length str)))
                                        str)) 
           (display "\" (length: ") (display (string-length str)) (display ")") (newline)
  
  (run-test-for-function "Original (char-by-char)" string-suffix?-original suffix str num-iterations)
  (run-test-for-function "Optimized (substring)" string-suffix?-optimized suffix str num-iterations)
  (run-test-for-function "SRFI-13 current" string-suffix? suffix str num-iterations)
  
  (newline))

;; --- Test Data Generation ---
(define (make-test-string length char)
  (make-string length char))

(define (make-mixed-string length)
  (list->string
    (map (lambda (i) 
           (integer->char (+ (char->integer #\a) (modulo i 26))))
         (iota length))))

;; Simple iota implementation if not available
(define (iota count . rest)
  (let ((start (if (null? rest) 0 (car rest)))
        (step (if (or (null? rest) (null? (cdr rest))) 1 (cadr rest))))
    (let loop ((i 0) (result '()))
      (if (>= i count)
          (reverse result)
          (loop (+ i 1) (cons (+ start (* i step)) result))))))

;; --- Main Test Execution ---
(define (run-all-tests . num-iterations-arg)
  (let ((num-iterations (if (and (pair? num-iterations-arg) 
                                 (integer? (car num-iterations-arg)) 
                                 (> (car num-iterations-arg) 0))
                            (car num-iterations-arg)
                            100000))) ; Default iterations

    (display "Starting performance tests for string-suffix? implementations...") (newline)
    (display "Using ") (display num-iterations) (display " iterations per test case.") (newline)
    (newline)

    ;; Test 1: Short suffix, short string (match)
    (execute-test-case
      "Short suffix, short string (match)"
      "lo"
      "hello"
      num-iterations)

    ;; Test 2: Short suffix, short string (no match)
    (execute-test-case
      "Short suffix, short string (no match)"
      "hi" 
      "hello"
      num-iterations)

    ;; Test 3: Empty suffix
    (execute-test-case
      "Empty suffix"
      ""
      "hello world"
      num-iterations)

    ;; Test 4: Long suffix, long string (match)
    (let ((long-string (make-mixed-string 1000)))
      (execute-test-case
        "Long suffix, long string (match)"
        (substring long-string 900 1000)
        long-string
        num-iterations))

    ;; Test 5: Long suffix, long string (no match - differs at start)
    (let ((long-string (make-mixed-string 1000)))
      (execute-test-case
        "Long suffix, long string (no match - differs at end)"
        "xyz"
        long-string
        num-iterations))

    ;; Test 6: Long suffix, long string (no match - differs at beginning of suffix)
    (let* ((long-string (make-mixed-string 1000))
           (almost-suffix (string-append "X" (substring long-string 901 1000))))
      (execute-test-case
        "Long suffix, long string (no match - differs at beginning of suffix)"
        almost-suffix
        long-string
        num-iterations))

    ;; Test 7: Very long strings
    (let ((very-long-string (make-mixed-string 10000)))
      (execute-test-case
        "Very long suffix, very long string (match)"
        (substring very-long-string 9000 10000)
        very-long-string
        (/ num-iterations 10))) ; Fewer iterations for very long strings

    ;; Test 8: Pathological case - suffix same length as string
    (execute-test-case
      "Suffix same length as string (match)"
      "hello"
      "hello"
      num-iterations)

    ;; Test 9: Suffix longer than string
    (execute-test-case
      "Suffix longer than string"
      "hello world"
      "hello"
      num-iterations)

    ;; Test 10: Common file extension pattern
    (execute-test-case
      "File extension pattern (match)"
      ".txt"
      "document.txt"
      num-iterations)

    ;; Test 11: URL pattern
    (execute-test-case
      "URL pattern (match)"
      ".com"
      "www.example.com"
      num-iterations)

    ;; Test 12: Long common suffix
    (execute-test-case
      "Long common suffix (match)"
      "tion"
      "internationalization"
      num-iterations)

    (display "Performance tests completed.") (newline)))

;; To run the tests, call:
;; (run-all-tests)
;; or with custom iteration count:
;; (run-all-tests 50000)

(run-all-tests) 
