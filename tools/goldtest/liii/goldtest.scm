
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

(define-library (liii goldtest)
  (export run-goldtest)
  (import (scheme base)
          (scheme process-context)
          (liii list)
          (liii string)
          (liii os)
          (liii path))
  (begin

    (define ESC (string #\escape #\[))

    (define (color code)
      (string-append ESC (number->string code) "m"))

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
                (loop (string-append result sep part) (cdr rest)))))))

    (define (find-test-files dir)
      (let ((files '()))
        (when (path-dir? dir)
          (let ((entries (listdir dir)))
            (for-each
              (lambda (entry)
                (let ((full-path (test-path-join dir entry)))
                  (cond
                    ((path-dir? full-path)
                     (set! files (append files (find-test-files full-path))))
                    ((and (path-file? full-path)
                          (string-ends-with? entry "-test.scm"))
                     (set! files (cons full-path files))))))
              entries)))
        files))

    (define (goldfish-cmd)
      (if (os-windows?)
        "bin\\gf -m r7rs "
        "bin/gf -m r7rs "))

    (define (run-test-file test-file)
      (let ((cmd (string-append (goldfish-cmd) test-file)))
        (display "----------->") (newline)
        (display cmd) (newline)
        (let ((result (os-call cmd)))
          (cons test-file result))))

    (define (display-summary test-results)
      (let ((total (length test-results))
            (passed (count (lambda (x) (zero? (cdr x))) test-results))
            (failed (- (length test-results)
                       (count (lambda (x) (zero? (cdr x))) test-results))))
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

    (define (run-goldtest)
      (let ((test-all-path (test-path-join "tests" "test_all.scm")))
        (if (path-file? test-all-path)
          ; 如果存在 test_all.scm，则运行它
          (begin
            (display (string-append YELLOW "Found test_all.scm, running it..." RESET))
            (newline)
            (newline)
            (let ((cmd (string-append (goldfish-cmd) test-all-path)))
              (display cmd) (newline)
              (let ((result (os-call cmd)))
                (newline)
                (display "=== Summary ===") (newline)
                (display "  test_all.scm ... ")
                (if (zero? result)
                  (display (string-append GREEN "PASS" RESET))
                  (display (string-append RED "FAIL" RESET)))
                (newline)
                (exit result))))
          ; 否则运行所有 xxx-test.scm 文件
          (let ((test-files (sort (find-test-files "tests") string<?)))
            (if (null? test-files)
              (begin
                (display (string-append YELLOW "No test files found in tests directory" RESET))
                (newline)
                (exit 0))
              (let ((test-results
                      (fold (lambda (test-file acc)
                              (newline)
                              (cons (run-test-file test-file) acc))
                            (list)
                            test-files)))
                (let ((failed (display-summary test-results)))
                  (exit (if (> failed 0) -1 0)))))))))
)
