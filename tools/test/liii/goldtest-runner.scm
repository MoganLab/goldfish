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

(define-library (liii goldtest-runner)
  (import (scheme base)
    (scheme file)
    (liii list)
    (liii os)
    (liii path)
    (liii sys)
    (liii par)
  ) ;import
  (export ESC color GREEN RED YELLOW RESET goldfish-cmd run-test-file
    display-fail-log cleanup-log run-tests-parallel failed-test-files
    display-summary
  ) ;export
  (begin

    (define ESC (string #\escape #\[))

    (define (color code)
      (string-append ESC (number->string code) "m")
    ) ;define

    (define GREEN (color 32))
    (define RED (color 31))
    (define YELLOW (color 33))
    (define RESET (color 0))

    (define (goldfish-cmd)
      (string-append (executable) " -m r7rs ")
    ) ;define

    (define (run-test-file test-file)
      (let ((cmd (string-append (goldfish-cmd) test-file)))
        (display "----------->")
        (newline)
        (display cmd)
        (newline)
        (flush-output-port)
        (let ((result (os-call cmd)))
          (cons test-file result)
        ) ;let
      ) ;let
    ) ;define

    (define (display-fail-log tmp-log)
      (when (file-exists? tmp-log)
        (let ((port (open-input-file tmp-log)))
          (let loop
            ()
            (let ((line (read-line port)))
              (unless (eof-object? line)
                (display "    ")
                (display line)
                (newline)
                (loop)
              ) ;unless
            ) ;let
          ) ;let
          (close-input-port port)
          (remove tmp-log)
        ) ;let
      ) ;when
    ) ;define

    (define (cleanup-log tmp-log)
      (when (file-exists? tmp-log)
        (remove tmp-log)
      ) ;when
    ) ;define

    (define (report-one-test-result idx total res)
      (let ((test-file (car res)) (exit-code (cadr res)) (log-path (caddr res)))
        (display (string-append "  ["
                   (number->string idx)
                   "/"
                   (number->string total)
                   "] "
                   test-file
                   " ... "
                 ) ;string-append
        ) ;display
        (if (zero? exit-code)
          (begin
            (display (string-append GREEN "PASS" RESET "\n"))
            (cleanup-log log-path)
          ) ;begin
          (begin
            (display (string-append RED "FAIL" RESET " exit-code=" (number->string exit-code) "\n")
            ) ;display
            (display-fail-log log-path)
          ) ;begin
        ) ;if
      ) ;let
    ) ;define

    (define (run-tests-parallel test-files jobs)
      (let* ((total (length test-files)) (exe (executable)))
        (display (string-append "Running "
                   (number->string total)
                   " tests in parallel with "
                   (number->string jobs)
                   " workers..."
                 ) ;string-append
        ) ;display
        (newline)
        (newline)
        (let* ((vec (list->vector test-files))
               (run-one-test
                 (lambda (test-file)
                   (let* ((cmd (string-append exe " -m r7rs " test-file)) (exit-code (os-call cmd)))
                     (list test-file exit-code "")
                   ) ;let*
                 ) ;lambda
               ) ;run-one-test
               (raw-results (vector-par-map run-one-test vec jobs))
              ) ;
          (let ((idx 0))
            (vector-for-each (lambda (res) (set! idx (+ idx 1)) (report-one-test-result idx total res))
              raw-results
            ) ;vector-for-each
          ) ;let
          (vector->list
            (vector-map (lambda (r) (cons (car r) (cadr r))) raw-results)
          ) ;vector->list
        ) ;let*
      ) ;let*
    ) ;define

    (define (failed-test-files test-results)
      (map car
        (filter
          (lambda (test-result) (not (zero? (cdr test-result))))
          test-results
        ) ;filter
      ) ;map
    ) ;define

    (define (display-summary test-results)
      (let* ((total (length test-results))
             (passed
               (count (lambda (x) (zero? (cdr x))) test-results)
             ) ;passed
             (failed-files (failed-test-files test-results))
             (failed (- total passed))
            ) ;
        (newline)
        (display "=== Test Summary ===")
        (newline)
        (newline)
        (for-each
          (lambda (test-result)
            (let ((test-file (car test-result)) (exit-code (cdr test-result)))
              (display (string-append "  " test-file " ... "))
              (if (zero? exit-code)
                (display (string-append GREEN "PASS" RESET))
                (display (string-append RED "FAIL" RESET " exit-code=" (number->string exit-code))
                ) ;display
              ) ;if
              (newline)
            ) ;let
          ) ;lambda
          test-results
        ) ;for-each
        (newline)
        (display "=== Summary ===")
        (newline)
        (display (string-append "  Total:  " (number->string total)))
        (newline)
        (display (string-append "  " GREEN "Passed: " (number->string passed) RESET))
        (newline)
        (when (> failed 0)
          (display (string-append "  " RED "Failed: " (number->string failed) RESET))
          (newline)
          (display "  Failed Test Files:")
          (newline)
          (for-each (lambda (test-file) (display (string-append "    " test-file)) (newline))
            failed-files
          ) ;for-each
        ) ;when
        (newline)
        failed
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
