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

(define-library (liii goldtest-args)
  (import (scheme base)
    (scheme process-context)
    (liii string)
    (liii argparse)
    (liii os)
    (liii path)
  ) ;import
  (export make-test-arg-parser classify-test-arg parse-test-args
    parse-test-changed-since parse-test-jobs parse-test-all test-help-requested?
    show-help split-tests-target check-and-switch-to-target
  ) ;export
  (begin

    (define (make-test-arg-parser)
      (let ((parser (make-argument-parser '((command . "test")
                                            (skip-value-options "-m" "--mode")
                                            (skip-prefix-options "-m="
                                              "--mode=")
                                            (unknown-options . positional))
                    ) ;make-argument-parser
            ) ;parser
           ) ;
        (parser :add-argument '((name . "changed-since") (type . string)))
        (parser :add-argument '((name . "all") (action . store-true)))
        (parser
          :add-argument
          '((name . "jobs") (short . "j") (type . number) (default . 8))
        ) ;parser
        (parser :add-argument '((name . "help")
                                (short . "h")
                                (action . store-true)))
        parser
      ) ;let
    ) ;define

    (define (classify-test-arg arg)
      (cond
       ((or (string-contains arg "/") (and (os-windows?) (string-contains arg "\\")))
        (let ((abs-path (if (path-absolute? arg) arg (path-join (getcwd) arg))))
          (cond ((path-file? abs-path) (cons 'file arg))
                ((path-dir? abs-path) (cons 'dir arg))
                (else (cons 'pattern arg))
          ) ;cond
        ) ;let
       ) ;
       ((string-ends? arg ".scm") (cons 'filename arg))
       (else (cons 'pattern arg))
      ) ;cond
    ) ;define

    (define (parse-test-args args)
      (let ((parser (make-test-arg-parser)))
        (parser :parse-argv args)
        (let ((positionals (parser :positionals)))
          (if (null? positionals) (cons #f #f) (classify-test-arg (car positionals)))
        ) ;let
      ) ;let
    ) ;define

    (define (parse-test-changed-since args)
      (let ((parser (make-test-arg-parser)))
        (parser :parse-argv args)
        (parser 'changed-since)
      ) ;let
    ) ;define

    (define (parse-test-jobs args)
      (let ((parser (make-test-arg-parser)))
        (parser :parse-argv args)
        (let ((j (parser 'jobs)))
          (if (and (number? j) (> j 0)) (exact j) 1)
        ) ;let
      ) ;let
    ) ;define

    (define (parse-test-all args)
      (let ((parser (make-test-arg-parser)))
        (parser :parse-argv args)
        (parser 'all)
      ) ;let
    ) ;define

    (define (split-tests-target target)
      (let ((marker-length 6))
        (let loop
          ((i 0))
          (cond
           ((> i (- (string-length target) marker-length)) #f)
           ((and
              (or (string=? (substring target i (+ i marker-length)) "/tests")
                (string=? (substring target i (+ i marker-length)) "\\tests")
              ) ;or
              (or (= (+ i marker-length) (string-length target))
                (char=? (string-ref target (+ i marker-length)) #\/)
                (char=? (string-ref target (+ i marker-length)) #\\)
              ) ;or
            ) ;and
            (let ((parent (substring target 0 i)) (next-pos (+ i marker-length)))
              (if (> (string-length parent) 0)
                (let ((tests-path (substring target (+ i 1))))
                  (if (= next-pos (string-length target))
                    (cons parent (string-append tests-path (string (string-ref target i))))
                    (cons parent tests-path)
                  ) ;if
                ) ;let
                #f
              ) ;if
            ) ;let
           ) ;
           (else (loop (+ i 1)))
          ) ;cond
        ) ;let
      ) ;let
    ) ;define

    (define (switch-dir-if-target target args)
      (let ((tests-target (split-tests-target target)))
        (cond
          (tests-target
            (let ((parent (car tests-target)) (tests-path (cdr tests-target)))
              (if (path-dir? parent)
                (begin
                  (chdir parent)
                  (cons (car args)
                    (map (lambda (arg) (if (equal? arg target) tests-path arg)) (cdr args))
                  ) ;cons
                ) ;begin
                args
              ) ;if
            ) ;let
          ) ;tests-target
          ((and (path-dir? target)
             (path-dir? (if (os-windows?)
                          (string-append target "\\tests")
                          (string-append target "/tests")
                        ) ;if
             ) ;path-dir?
           ) ;and
           (chdir target)
           (cons (car args)
             (map (lambda (arg) (if (equal? arg target) "tests" arg)) (cdr args))
           ) ;cons
          ) ;
          (else args)
        ) ;cond
      ) ;let
    ) ;define

    (define (check-and-switch-to-target args)
      (let loop
        ((remaining (cdr args)) (found-target #f))
        (cond ((null? remaining)
               (if found-target (switch-dir-if-target found-target args) args)
              ) ;
              ((equal? (car remaining) "test") (loop (cdr remaining) found-target))
              ((or (equal? (car remaining) "-m")
                 (equal? (car remaining) "--mode")
                 (equal? (car remaining) "--changed-since")
               ) ;or
               (loop (if (null? (cdr remaining)) '() (cddr remaining)) found-target)
              ) ;
              ((or (string-starts? (car remaining) "-m=")
                 (string-starts? (car remaining) "--mode=")
                 (string-starts? (car remaining) "--changed-since=")
                 (equal? (car remaining) "--all")
               ) ;or
               (loop (cdr remaining) found-target)
              ) ;
              ((not found-target) (loop (cdr remaining) (car remaining)))
              (else (loop (cdr remaining) found-target))
        ) ;cond
      ) ;let
    ) ;define

    (define (show-help)
      (display (string-append "gf test - Goldfish Scheme Test Runner\n\n"
                 "Usage:\n" "  gf test [options] [PATH|PATTERN]\n\n"
                 "Options:\n"
                 "  -j, --jobs NUM                   Number of parallel worker jobs (default: 8)\n"
                 "  --all                            Run all tests (greedy: changed first, then all)\n"
                 "  --changed-since REV              Run tests changed since REV\n\n"
                 "Examples:\n"
                 "  gf test                          Run tests (smart route based on git branch)\n"
                 "  gf test --all                    Run all tests\n"
                 "  gf test tools/doc/tests/         Run tests in directory\n"
                 "  gf test tests/liii/string/       Run tests in directory\n"
                 "  gf test string-test.scm          Run specific test file\n"
                 "  gf test string                   Run tests matching pattern\n"
                 "  gf test --changed-since=HEAD     Run changed test files\n\n"
                 "Note:\n"
                 "  Smart routing: on non-main branch, gf test runs --changed-since=main\n"
                 "  If path contains /tests/, it will switch to parent directory\n"
                 "  e.g., gf test tools/doc/tests/  =>  cd tools/doc && gf test tests/\n"
               ) ;string-append
      ) ;display
    ) ;define

    (define (test-help-requested? args)
      (let ((parser (make-test-arg-parser)))
        (parser :parse-argv args)
        (parser 'help)
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
