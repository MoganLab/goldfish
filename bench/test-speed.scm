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
  (liii string)
  (liii vector)
  (liii list)
) ;import

;; --- Globals accessed by the functions under test ---
(define digits #())
(define dlen 9)

;; --- User's original functions (to be tested) ---
(define (build-digits-str-2)
  (let ((digits-str ($ ($ (reverse digits)
                         :map
                         (lambda (x) (box (number->string x)))
                         :map
                         (@ _ :pad-left 9 #\0)
                         :make-string
                       ) ;$
                      :drop-while
                      (@ _ :equals (rich-char #\0))
                      :get
                    ) ;$
        ) ;digits-str
       ) ;
    (if (string-null? digits-str)
      "0"
      digits-str
    ) ;if
  ) ;let
) ;define

(define (build-digits-str)
  (let loop
    ((i (- (vector-length digits) 1))
     (result "")
     (len (vector-length digits))
    ) ;
    (if (< i 0)
      result
      (let ((digit-str (cond
                         ;; 最高位（向量的最后一位），无需前导零
                         ((= i (- len 1))
                          (number->string (vector-ref digits i))
                         ) ;
                         ;; 其他位，需要前导零填充到dlen长度
                         (else (let ((str (number->string (vector-ref digits i))
                                     ) ;str
                                    ) ;
                                 (string-pad str dlen #\0)
                               ) ;let
                         ) ;else
                       ) ;cond
            ) ;digit-str
           ) ;
        (loop (- i 1)
          (string-append result digit-str)
          len
        ) ;loop
      ) ;let
    ) ;if
  ) ;let
) ;define

(define (build-digits-str-optimized)
  (let ((len (vector-length digits)))
    (if (zero? len)
      ""
      (let ((port (open-output-string)))
        (do ((i (- len 1) (- i 1)))
          ((< i 0))
          (display (cond ((= i (- len 1))
                          (number->string (vector-ref digits i))
                         ) ;
                         (else (let ((s (number->string (vector-ref digits i))
                                     ) ;s
                                    ) ;
                                 (string-pad s dlen #\0)
                               ) ;let
                         ) ;else
                   ) ;cond
            port
          ) ;display
        ) ;do
        (get-output-string port)
      ) ;let
    ) ;if
  ) ;let
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
          num-iterations
          print-sample?
        ) ;run-test-for-function
  (display "  Testing function for: ")
  (display desc)
  (newline)
  (if (and (defined? 'flush-output-port)
        (port? (current-output-port))
      ) ;and
    (flush-output-port (current-output-port)
    ) ;flush-output-port
  ) ;if

  (let ((total-time 0) (sample-output #f))
    (do ((i 0 (+ i 1)))
      ((= i num-iterations))
      (let-values (((run-output elapsed-time)
                    (time-thunk func)
                   ) ;
                  ) ;
        (set! total-time
          (+ total-time elapsed-time)
        ) ;set!
        (if (and print-sample? (= i 0))
          (set! sample-output run-output)
        ) ;if
      ) ;let-values
    ) ;do

    (if print-sample?
      (begin
        (display "    Sample output: \"")
        (display sample-output)
        (display "\"")
        (newline)
      ) ;begin
    ) ;if
    (display "    Total time: ")
    (display total-time)
    (display " jiffies for ")
    (display num-iterations)
    (display " iterations.")
    (newline)
    (display "    Average time: ")
    (display (if (> num-iterations 0)
               (/ total-time num-iterations)
               0
             ) ;if
    ) ;display
    (display " jiffies/iteration.")
    (newline)
    (newline)
    (if (and (defined? 'flush-output-port)
          (port? (current-output-port))
        ) ;and
      (flush-output-port (current-output-port)
      ) ;flush-output-port
    ) ;if
  ) ;let
) ;define

(define (execute-test-case case-name
          test-digits
          test-dlen
          num-iterations
        ) ;execute-test-case
  (display "Running Test Case: ")
  (display case-name)
  (newline)
  (display "  Input digits: ")
  (write test-digits)
  (newline)
  (display "  Input dlen: ")
  (display test-dlen)
  (newline)

  (set! digits test-digits)
  (set! dlen test-dlen)

  (run-test-for-function "build-digits-str-2"
    build-digits-str-2
    num-iterations
    #f
  ) ;run-test-for-function
  (run-test-for-function "build-digits-str"
    build-digits-str
    num-iterations
    #f
  ) ;run-test-for-function
  (run-test-for-function "build-digits-str-optimized"
    build-digits-str-optimized
    num-iterations
    #f
  ) ;run-test-for-function

  (newline)
  (if (and (defined? 'flush-output-port)
        (port? (current-output-port))
      ) ;and
    (flush-output-port (current-output-port)
    ) ;flush-output-port
  ) ;if
) ;define

;; --- Main Test Execution ---
(define (run-all-tests . num-iterations-arg)
  (let ((num-iterations (if (and (pair? num-iterations-arg)
                              (integer? (car num-iterations-arg))
                              (> (car num-iterations-arg) 0)
                            ) ;and
                          (car num-iterations-arg)
                          10000
                        ) ;if
        ) ;num-iterations
       ) ;

    (display "Starting performance tests for build-digits-str variants..."
    ) ;display
    (newline)
    (display "Using ")
    (display num-iterations)
    (display " iterations per function per test case."
    ) ;display
    (newline)
    (newline)

    (execute-test-case "Moderate Sized Number"
      (vector 0 0 345678901 456789012 123)
      9
      num-iterations
    ) ;execute-test-case

    (execute-test-case "Moderate Sized Number (10 Chunks)"
      (vector 0
        0
        345678901
        456789012
        123
        2319
        10
        23
        1111
        0
      ) ;vector
      9
      num-iterations
    ) ;execute-test-case

    (execute-test-case "Single Chunk Number (MSD)"
      (vector 12345)
      9
      num-iterations
    ) ;execute-test-case

    ;; Helper to create a list of numbers for the large test case
    ;; (define (iota count start step)
    ;;   (if (<= count 0)
    ;;       '()
    ;;       (cons start (iota (- count 1) (+ start step) step))))
    ;; Removed iota definition to keep it simpler, assuming you can create this vector manually
    ;; or have (srfi 1) iota available
    (execute-test-case "Large Number of Chunks (20 Chunks)"
      ;; Manually create or use (list->vector (iota 20 1 100000000)) if iota is available
      (vector 1
        100000001
        200000001
        300000001
        400000001
        500000001
        600000001
        700000001
        800000001
        900000001
        1000000001
        1100000001
        1200000001
        1300000001
        1400000001
        1500000001
        1600000001
        1700000001
        1800000001
        1900000001
      ) ;vector
      9
      num-iterations
    ) ;execute-test-case

    ;; 新增：块为50的测试用例
    (let ((fifty-chunks-vector (make-vector 50)))
      ;; 用一些示例数据填充这个向量
      ;; 这里我们让每个块的值略有不同，以模拟真实数据
      (do ((k 0 (+ k 1)))
        ((= k 50))
        ;; (vector-set! vector index value)
        (vector-set! fifty-chunks-vector
          k
          (+ 100000000 (* k 12345))
        ) ;vector-set!
      ) ;do

      (execute-test-case "Large Number of Chunks (50 Chunks)"
        fifty-chunks-vector
        9
        num-iterations
      ) ;execute-test-case
    ) ;let

    (let ((1000-chunks-vector (make-vector 1000))
         ) ;
      ;; 用一些示例数据填充这个向量
      ;; 这里我们让每个块的值略有不同，以模拟真实数据
      (do ((k 0 (+ k 1)))
        ((= k 1000))
        ;; (vector-set! vector index value)
        (vector-set! 1000-chunks-vector k 1)
      ) ;do

      (execute-test-case "Large Number of Chunks (1000 Chunks)"
        1000-chunks-vector
        9
        num-iterations
      ) ;execute-test-case
    ) ;let

    (execute-test-case "All Zero Chunks"
      (vector 0 0 0)
      9
      num-iterations
    ) ;execute-test-case

    (execute-test-case "Empty Digits Vector"
      (vector)
      9
      num-iterations
    ) ;execute-test-case

    (display "Performance tests completed.")
    (newline)
    (if (and (defined? 'flush-output-port)
          (port? (current-output-port))
        ) ;and
      (flush-output-port (current-output-port)
      ) ;flush-output-port
    ) ;if
  ) ;let
) ;define

;; To run:
;; (run-all-tests)          ; Uses default 10000 iterations
;; (run-all-tests 50000)    ; Uses 50000 iterations

(run-all-tests 1000)
(newline)
(display (jiffies-per-second))
(newline)
