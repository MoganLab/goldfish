
;
; Copyright (C) 2024 The Goldfish Scheme Authors
;
; Licensed under the Apache License, Version 2.0 (the "License");
; you may not use this file except in compliance with the License.
; You may obtain a copy of the License at
;
; http://www.apache.org/licenses/LICENSE-2.0
;
; Unless required by applicable law or agreed to in writing, software
; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
; License for the specific language governing permissions and limitations
; under the License.
;

(import (scheme base)
        (liii sort)
        (liii list)
        (liii string)
        (liii os)
        (liii path)
        (liii sys)
) ;import

(define ESC (string #\escape #\[))

(define (color code)
  (string-append ESC (number->string code) "m")
) ;define

(define GREEN (color 32))
(define RED (color 31))
(define YELLOW (color 33))
(define RESET (color 0))

(define (test-path-join . parts)
  (let ((sep (string (os-sep))))
    (let loop ((result "")
               (rest parts))
      (if (null? rest)
        result
        (let ((part (car rest)))
          (if (string-null? result)
            (loop part (cdr rest))
            (loop (string-append result sep part) (cdr rest))
          ) ;if
        ) ;let
      ) ;if
    ) ;let
  ) ;let
) ;define

(define (http-test-enabled?)
  (let ((env-var (getenv "GOLDFISH_TEST_HTTP")))
    (and env-var (not (equal? env-var "0")))
  ) ;let
) ;define

(define (stacktrace-test-enabled?)
  (let ((env-var (getenv "GOLDFISH_TEST_STACKTRACE")))
    (and env-var (not (equal? env-var "0")))
  ) ;let
) ;define

(define (should-run-test? entry)
  (cond
    ((string-contains entry "http-test")
     (http-test-enabled?)
    ) ;
    ((string-contains entry "srfi-78-simple-stacktrace")
     (stacktrace-test-enabled?)
    ) ;
    (else
     #t
    ) ;else
  ) ;cond
) ;define

(define (find-test-files dir)
  (let ((files '()))
    (when (path-dir? dir)
      (let ((entries (listdir dir)))
        (for-each
          (lambda (entry)
            (let ((full-path (test-path-join dir entry)))
              (cond
                ((path-dir? full-path)
                 (set! files (append files (find-test-files full-path)))
                ) ;
                ((and (path-file? full-path)
                      (string-ends? entry "-test.scm")
                      (should-run-test? entry))
                 (set! files (cons full-path files))
                ) ;
              ) ;cond
            ) ;let
          ) ;lambda
          entries
        ) ;for-each
      ) ;let
    ) ;when
    files
  ) ;let
) ;define

(define (goldfish-cmd)
  (string-append (executable) " -m r7rs ")
) ;define

(define (run-test-file test-file)
  (let ((cmd (string-append (goldfish-cmd) test-file)))
    (display "----------->") (newline)
    (display cmd) (newline)
    (let ((result (os-call cmd)))
      (cons test-file result)
    ) ;let
  ) ;let
) ;define

(define (display-summary test-results)
  (let ((total (length test-results))
        (passed (count (lambda (x) (zero? (cdr x))) test-results))
        (failed (- (length test-results)
                   (count (lambda (x) (zero? (cdr x))) test-results)))
        ) ;failed
    (newline)
    (display "=== Test Summary ===") (newline)
    (newline)
    (for-each
      (lambda (test-result)
        (let ((test-file (car test-result))
              (exit-code (cdr test-result)))
          (display (string-append "  " test-file " ... "))
          (if (zero? exit-code)
            (display (string-append GREEN "PASS" RESET))
            (display (string-append RED "FAIL" RESET))
          ) ;if
          (newline)
        ) ;let
      ) ;lambda
      test-results
    ) ;for-each
    (newline)
    (display "=== Summary ===") (newline)
    (display (string-append "  Total:  " (number->string total))) (newline)
    (display (string-append "  " GREEN "Passed: " (number->string passed) RESET)) (newline)
    (when (> failed 0)
      (display (string-append "  " RED "Failed: " (number->string failed) RESET)) (newline)
    ) ;when
    (newline)
    failed
  ) ;let
) ;define

(define (run-goldtest)
  (let ((test-files (list-sort string<? (find-test-files "tests"))))
    (if (null? test-files)
      (begin
        (display (string-append YELLOW "No test files found in tests directory" RESET))
        (newline)
        (exit 0)
      ) ;begin
      (let ((test-results
              (fold (lambda (test-file acc)
                      (newline)
                      (cons (run-test-file test-file) acc))
                    (list)
                    test-files))
              ) ;fold
        (let ((failed (display-summary test-results)))
          (exit (if (> failed 0) -1 0))
        ) ;let
      ) ;let
    ) ;if
  ) ;let
) ;define
