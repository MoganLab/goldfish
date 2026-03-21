
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
  (let* ((sep (string (os-sep))))
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

(define (find-test-files dir)
  (if (path-dir? dir)
    (let ((entries (listdir dir)))
      (flat-map
        (lambda (entry)
          (let ((full-path (test-path-join dir entry)))
            (cond
              ((path-dir? full-path)
               (find-test-files full-path))
              ((and (path-file? full-path)
                    (string-ends? entry "-test.scm"))
               (list full-path))
              (else '()))))
        entries))
    '()))

(define (goldfish-cmd)
  (string-append (executable) " -m r7rs ")
) ;define

(define (run-test-file test-file)
  (let* ((cmd (string-append (goldfish-cmd) test-file))
         (result (begin
                   (display "----------->") (newline)
                   (display cmd) (newline)
                   (os-call cmd))))
    (cons test-file result))
) ;define

(define (display-summary test-results)
  (let* ((total (length test-results))
         (passed (count (lambda (x) (zero? (cdr x))) test-results))
         (failed (- total passed)))
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
            (display (string-append RED "FAIL" RESET)))
          (newline)))
      test-results)
    (newline)
    (display "=== Summary ===") (newline)
    (display (string-append "  Total:  " (number->string total))) (newline)
    (display (string-append "  " GREEN "Passed: " (number->string passed) RESET)) (newline)
    (when (> failed 0)
      (display (string-append "  " RED "Failed: " (number->string failed) RESET)) (newline))
    (newline)
    failed))
) ;define

(define (run-goldtest)
  (let* ((test-files (list-sort string<? (find-test-files "tests")))
         (test-results (if (null? test-files)
                         '()
                         (fold (lambda (test-file acc)
                                 (newline)
                                 (cons (run-test-file test-file) acc))
                               (list)
                               test-files)))
         (failed (if (null? test-files)
                   0
                   (display-summary test-results))))
    (if (null? test-files)
      (begin
        (display (string-append YELLOW "No test files found in tests directory" RESET))
        (newline)
        (exit 0))
      (exit (if (> failed 0) -1 0))))
) ;define
