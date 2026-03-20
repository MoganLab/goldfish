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

(import (liii list)
        (liii string)
        (liii os)
        (liii path)
) ;import

(define enable-http-tests?
  (let ((env-var (getenv "GOLDFISH_TEST_HTTP")))
    (and env-var (not (equal? env-var "0")))
  ) ;let
) ;define

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

(define level1-tests
  (let* ((test-root (test-path-join "tests" "goldfish"))
         (subdirs (filter path-dir? (map (lambda (x) (test-path-join test-root x))
                                         (listdir test-root)))
         ) ;subdirs
         (all-files (flat-map (lambda (dir)
                                (map (lambda (f) (test-path-join dir f))
                                     (listdir dir))
                                ) ;map
                              subdirs)
         ) ;all-files
         (test-files (filter path-file? all-files)))
    (filter (lambda (file-path)
              (and (not (string-contains file-path "srfi-78"))
                   (or enable-http-tests?
                       (not (string-contains file-path "http-test")))
                   ) ;or
              ) ;and
            test-files
    ) ;filter
  ) ;let*
) ;define

(define (all-tests)
  level1-tests
) ;define

(define (goldfish-cmd)
  (if (os-windows?)
    "bin\\goldfish -m r7rs "
    "bin/goldfish -m r7rs "
  ) ;if
) ;define

(define ESC (string #\escape #\[))
(define (color code)
  (string-append ESC
                 (number->string code)
                 "m"
  ) ;string-append
) ;define

(define GREEN (color 32))
(define RED   (color 31))
(define RESET (color 0))

(let ((test-results
       (fold (lambda (test-file acc)
               (let ((cmd (string-append (goldfish-cmd) test-file)))
                 (newline)
                 (display "----------->") (newline)
                 (display cmd) (newline)
                 (let ((result (os-call cmd)))
                   (cons (cons cmd result) acc))
                 ) ;let
               ) ;let
             (list)
             (all-tests)))
       ) ;fold
  (newline)
  (display "=== Summary ===") (newline)
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
  (when (any (lambda (x) (not (zero? (cdr x)))) test-results)
    (exit -1)
  ) ;when
) ;let
