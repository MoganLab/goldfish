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

(define (%goldtest-common-dirname path-str)
  (let loop
    ((i (- (string-length path-str) 1)))
    (cond ((< i 0) ".")
          ((or (char=? (string-ref path-str i) #\/) (char=? (string-ref path-str i) #\\))
           (if (= i 0) "." (substring path-str 0 i))
          ) ;
          (else (loop (- i 1)))
    ) ;cond
  ) ;let
) ;define

(set! *load-path*
  (append (list "../common" "tools/common")
    (map (lambda (root) (string-append (%goldtest-common-dirname root) "/common"))
      *load-path*
    ) ;map
    *load-path*
  ) ;append
) ;set!

(define-library (liii goldtest)
  (import (scheme base)
    (scheme process-context)
    (liii sort)
    (liii list)
    (liii string)
    (liii goldtool-changed)
    (liii os)
    (liii path)
    (liii sys)
    (liii goldtest-args)
    (liii goldtest-route)
    (liii goldtest-runner)
    (liii format base)
    (liii format color)
  ) ;import
  (export parse-test-args parse-test-changed-since parse-test-jobs
    filter-test-files filter-changed-test-files find-test-files
    failed-test-files split-tests-target run-goldtest main
  ) ;export
  (begin

    (define (test-path-join . parts)
      (let ((sep (string (os-sep))))
        (let loop
          ((result "") (rest parts))
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
      (let ((files '()))
        (when (path-dir? dir)
          (let ((entries (listdir dir)))
            (for-each
              (lambda (entry)
                (let ((full-path (test-path-join dir entry)))
                  (cond
                   ((path-dir? full-path) (set! files (append files (find-test-files full-path))))
                   ((and (path-file? full-path) (string-ends? entry "-test.scm"))
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

    (define (filter-test-files test-files arg-type arg-value)
      (case arg-type
       ((file) (list arg-value))
       ((dir)
        (let ((dir-pattern (if (os-windows?) (string-replace arg-value "/" "\\") arg-value)))
          (filter (lambda (file) (string-starts? file dir-pattern)) test-files)
        ) ;let
       ) ;
       ((filename)
        (filter (lambda (file) (string=? (path-name file) arg-value)) test-files)
       ) ;
       ((pattern) (filter (lambda (file) (string-contains file arg-value)) test-files))
       (else test-files)
      ) ;case
    ) ;define

    (define (normalize-test-file-path file)
      (if (os-windows?) (string-replace file "\\" "/") file)
    ) ;define

    (define (filter-changed-test-files test-files since)
      (let ((changed-files (changed-scheme-files-since since)))
        (filter (lambda (file) (member (normalize-test-file-path file) changed-files))
          test-files
        ) ;filter
      ) ;let
    ) ;define

    (define (display-filter-info arg-type arg-value)
      (case arg-type
       ((file) (display (string-append "Running test file: " arg-value)) (newline))
       ((dir)
        (display (string-append "Running tests in directory: " arg-value))
        (newline)
       ) ;
       ((filename)
        (display (string-append "Running tests with file name: " arg-value))
        (newline)
       ) ;
       ((pattern)
        (display (string-append "Running tests matching pattern: " arg-value))
        (newline)
       ) ;
      ) ;case
    ) ;define

    (define (resolve-test-files filtered final-changed-since need-run-all)
      (let ((changed-test-files (if final-changed-since
                                  (filter-changed-test-files filtered final-changed-since)
                                  '()
                                ) ;if
            ) ;changed-test-files
           ) ;
        (if need-run-all
          (let ((rem (filter (lambda (f) (not (member f changed-test-files))) filtered)))
            (append changed-test-files rem)
          ) ;let
          (if final-changed-since changed-test-files filtered)
        ) ;if
      ) ;let
    ) ;define

    (define (handle-no-test-files final-changed-since arg-value)
      (cond (final-changed-since (show #t (as-yellow "No test files changed since " final-changed-since) "\n")
            ) ;final-changed-since
            (arg-value (show #t (as-yellow "No test files matching " arg-value) "\n"))
            (else (show #t (as-yellow "No test files found in tests directory") "\n"))
      ) ;cond
      (exit 0)
    ) ;define

    (define (execute-tests test-files jobs)
      (let ((test-results
              (if (> jobs 1)
                (run-tests-parallel test-files jobs)
                (fold (lambda (f acc) (newline) (cons (run-test-file f) acc)) (list) test-files)
              ) ;if
            ) ;test-results
           ) ;
        (let ((failed (display-summary test-results)))
          (exit (if (> failed 0) -1 0))
        ) ;let
      ) ;let
    ) ;define

    (define (run-goldtest)
      (let* ((raw-args (command-line))
             (args (check-and-switch-to-target raw-args))
             (all-mode (parse-test-all args))
             (changed-since (parse-test-changed-since args))
             (jobs (parse-test-jobs args))
             (parsed (parse-test-args args))
             (arg-type (car parsed))
             (arg-value (cdr parsed))
             (route-res (if (and (not arg-type) (not changed-since))
                          (route-test-command args all-mode)
                          #f
                        ) ;if
             ) ;route-res
             (final-since (if route-res (car route-res) changed-since))
             (need-run-all (if route-res (cdr route-res) #f))
             (all-tests (list-sort string<? (find-test-files "tests")))
             (filtered (filter-test-files all-tests arg-type arg-value))
             (test-files (resolve-test-files filtered final-since need-run-all))
            ) ;
        (if (null? test-files)
          (handle-no-test-files final-since arg-value)
          (begin
            (when arg-value
              (display-filter-info arg-type arg-value)
            ) ;when
            (when final-since
              (display (string-append "Running changed tests since: " final-since "\n"))
            ) ;when
            (execute-tests test-files jobs)
          ) ;begin
        ) ;if
      ) ;let*
    ) ;define

    (define (main)
      (let ((args (command-line)))
        (if (test-help-requested? args) (begin (show-help) (exit 0)) (run-goldtest))
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
