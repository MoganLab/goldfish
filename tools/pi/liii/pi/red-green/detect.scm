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

(define-library (liii pi red-green detect)
  (import (scheme base) (scheme char) (liii string) (liii list))
  (export pi-tdd-test-run-command? pi-tdd-parse-test-run-result
    pi-tdd-test-run-trigger pi-tdd-exit-code-from-text
  ) ;export
  (begin

    (define %test-runner-patterns
      '("vitest" "jest" "pytest" "go test" "cargo test" "phpunit" "mix test"
        "dotnet test" "mvn test" "gradle test" "npm test" "npm run test"
        "npx vitest" "npx jest" "bin/gf test" "gf test" "ctest" "make test"
        "xmake test")
    ) ;define

    (define (pi-tdd-test-run-command? command)
      (let ((lower (string-downcase (if (string? command) command ""))))
        (any (lambda (pattern) (string-contains? lower pattern)) %test-runner-patterns)
      ) ;let
    ) ;define

    (define (%trim-punctuation token)
      (let* ((token (string-trim-both token)) (n (string-length token)))
        (let loop
          ((start 0) (end n))
          (cond ((>= start end) "")
                ((char-punctuation? (string-ref token start)) (loop (+ start 1) end))
                ((char-punctuation? (string-ref token (- end 1))) (loop start (- end 1)))
                (else (substring token start end))
          ) ;cond
        ) ;let
      ) ;let*
    ) ;define

    (define (char-punctuation? ch)
      (string-contains? ",.;:|()[]{}" (string ch))
    ) ;define

    (define (%integer-token token)
      (let ((n (string->number (%trim-punctuation token))))
        (and n (exact-integer? n) n)
      ) ;let
    ) ;define

    (define (%count-keyword tokens keywords)
      (let loop
        ((rest tokens) (acc 0))
        (if (or (null? rest) (null? (cdr rest)))
          acc
          (let ((n (%integer-token (car rest)))
                (word (string-downcase (%trim-punctuation (cadr rest))))
               ) ;
            (if (and n (member word keywords))
              (loop (cdr rest) (+ acc n))
              (loop (cdr rest) acc)
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (%count-substring text sub)
      (let ((n (string-length sub)) (len (string-length text)))
        (let loop
          ((i 0) (acc 0))
          (cond ((> (+ i n) len) acc)
                ((string=? (substring text i (+ i n)) sub) (loop (+ i n) (+ acc 1)))
                (else (loop (+ i 1) acc))
          ) ;cond
        ) ;let
      ) ;let
    ) ;define

    (define (%go-output? text)
      (or (> (%count-substring text "--- PASS:") 0)
        (> (%count-substring text "--- FAIL:") 0)
        (string-contains? text "go: downloading")
        (%any-line-prefix? text '("ok " "ok\t" "FAIL" "FAIL\t"))
      ) ;or
    ) ;define

    (define (%line-prefix? line prefix)
      (and (>= (string-length line) (string-length prefix))
        (string=? (substring line 0 (string-length prefix)) prefix)
      ) ;and
    ) ;define

    (define (%any-line-prefix? text prefixes)
      (any
        (lambda (line) (any (lambda (prefix) (%line-prefix? line prefix)) prefixes))
        (string-split text "\n")
      ) ;any
    ) ;define

    (define (%parse-go-result text exit-code)
      (let* ((passed (%count-substring text "--- PASS:"))
             (failed (%count-substring text "--- FAIL:"))
             (errors
               (if (or (string-contains? text "panic:")
                     (string-contains? (string-downcase text) "build failed")
                   ) ;or
                 1
                 0
               ) ;if
             ) ;errors
             (passed
               (if (and (= passed 0) (%any-line-prefix? text '("ok " "ok\t"))) 1 passed)
             ) ;passed
             (failed
               (if (and (= failed 0) (= passed 0) (%any-line-prefix? text '("FAIL"
                                                                            "FAIL\t")))
                 1
                 failed
               ) ;if
             ) ;failed
            ) ;
        (when (and (= passed 0) (= failed 0) (= errors 0) (not (= exit-code 0)))
          (set! errors 1)
        ) ;when
        (if (and (= passed 0) (= failed 0) (= errors 0))
          #f
          (list (cons 'passed passed)
            (cons 'failed failed)
            (cons 'errors errors)
            (cons 'exit-code exit-code)
          ) ;list
        ) ;if
      ) ;let*
    ) ;define

    (define (%normalize-whitespace text)
      (string-replace (string-replace text "\n" " ") "\t" " ")
    ) ;define

    (define (%parse-counts-result text exit-code)
      (let* ((tokens (string-tokenize (%normalize-whitespace text)))
             (passed (%count-keyword tokens '("passed" "pass" "passes")))
             (failed (%count-keyword tokens '("failed" "fail" "failures")))
             (errors (%count-keyword tokens '("errors" "error")))
            ) ;
        (when (and (= passed 0) (= failed 0) (= errors 0) (not (= exit-code 0)))
          (set! errors 1)
        ) ;when
        (if (and (= passed 0) (= failed 0) (= errors 0))
          #f
          (list (cons 'passed passed)
            (cons 'failed failed)
            (cons 'errors errors)
            (cons 'exit-code exit-code)
          ) ;list
        ) ;if
      ) ;let*
    ) ;define

    (define (pi-tdd-parse-test-run-result text exit-code)
      (if (%go-output? text)
        (%parse-go-result text exit-code)
        (%parse-counts-result text exit-code)
      ) ;if
    ) ;define

    (define (pi-tdd-test-run-trigger run)
      (let ((failed (cdr (assoc 'failed run)))
            (errors (cdr (assoc 'errors run)))
            (passed (cdr (assoc 'passed run)))
           ) ;
        (cond ((and failed (> failed 0)) 'tests-fail)
              ((and errors (> errors 0)) 'tests-error)
              ((and passed (> passed 0)) 'tests-pass)
              (else #f)
        ) ;cond
      ) ;let
    ) ;define

    (define (%digits-at text start)
      (let loop
        ((i start) (acc '()))
        (if (and (< i (string-length text)) (char-numeric? (string-ref text i)))
          (loop (+ i 1) (cons (string-ref text i) acc))
          (if (null? acc) #f (string->number (list->string (reverse acc))))
        ) ;if
      ) ;let
    ) ;define

    (define (%index-of text sub start)
      (let ((n (string-length sub)) (len (string-length text)))
        (let loop
          ((i start))
          (cond ((> (+ i n) len) #f)
                ((string=? (substring text i (+ i n)) sub) i)
                (else (loop (+ i 1)))
          ) ;cond
        ) ;let
      ) ;let
    ) ;define

    (define (%find-exit-code text)
      (let ((marker "Command exited with code ")
            (idx (%index-of text "Command exited with code " 0))
           ) ;
        (if idx (%digits-at text (+ idx (string-length marker))) #f)
      ) ;let
    ) ;define

    (define (pi-tdd-exit-code-from-text text is-error?)
      (or (%find-exit-code text)
        (and is-error? (if (string-contains? text "Command timed out") -1 1))
        0
      ) ;or
    ) ;define

  ) ;begin
) ;define-library
