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
        (liii rich-path)
        (liii lang))

(define enable-http-tests? 
  (let ((env-var (getenv "GOLDFISH_TEST_HTTP")))
    (and env-var (not (equal? env-var "0")))))

(define level1-tests
  ((path :./ "tests" :/ "goldfish" :list-path)
   :filter (@ _ :dir?)
   :flat-map (lambda (x) ((x :list-path) :collect))
   :filter (@ _ :file?)
   :filter (lambda (x) 
             (let ((file-path (x :to-string)))
               (and (not ($ file-path :contains "srfi-78"))
                    (or enable-http-tests?
                        (not ($ file-path :contains "http-test"))))))
   :map (@ _ :to-string)))

(define (all-tests)
  level1-tests)

(define (goldfish-cmd)
  (if (os-windows?)
    "bin\\goldfish -m r7rs "
    "bin/goldfish -m r7rs "))

(define ESC (string #\escape #\[))
(define (color code)
  (string-append ESC
                 (number->string code)
                 "m"))

(define GREEN (color 32))
(define RED   (color 31))
(define RESET (color 0))

(let1 ret-l ($ ((all-tests)
                :map (lambda (x) (string-append (goldfish-cmd) x))
                :fold (list)
                      (lambda (cmd acc)
                       (newline)
                       (display "----------->") (newline)
                       (display cmd) (newline)
                       (let1 result (os-call cmd)
                         (cons (cons cmd result) acc)))))
  (newline)
  (display "=== Summary ===") (newline)
  (ret-l :for-each
         (lambda (test-result)
           (let ((test-file (car test-result))
                 (exit-code (cdr test-result)))
             (display (string-append "  " test-file " ... "))
             (if (zero? exit-code)
                 (display (string-append GREEN "PASS" RESET))
                 (display (string-append RED   "FAIL" RESET)))
             (newline))))
  (when (ret-l :exists (compose not zero? cdr))
    (exit -1)))
