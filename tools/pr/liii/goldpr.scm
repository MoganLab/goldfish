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

(define-library (liii goldpr)
  (import (scheme base)
    (scheme char)
    (scheme write)
    (scheme process-context)
    (liii argparse)
    (liii string)
    (liii subprocess)
  ) ;import
  (export main parse-pr-args run-pr pr-remote-url)
  (begin

    (define (pr-remote-url . opts)
      (let ((remote (if (null? opts) "origin" (car opts))))
        (let-values (((out err code)
                      (run-values (list 'git "remote" "get-url" remote) :stdout 'capture)
                     ) ;
                    ) ;
          (if (and (= code 0) (> (string-length out) 0)) (string-trim-both out) #f)
        ) ;let-values
      ) ;let
    ) ;define

    (define (pr-number-string? value)
      (and (string? value)
        (> (string-length value) 0)
        (let loop
          ((i 0))
          (or (= i (string-length value))
            (and (char-numeric? (string-ref value i)) (loop (+ i 1)))
          ) ;or
        ) ;let
      ) ;and
    ) ;define

    (define (parse-pr-args args)
      (let ((parser (make-argument-parser '((command . "pr")
                                            (skip-value-options "-m" "--mode"
                                              "-I" "-A")
                                            (skip-prefix-options "-m="
                                              "--mode=")
                                            (unknown-options . positional))
                    ) ;make-argument-parser
            ) ;parser
           ) ;
        (parser :parse-argv args)
        (let ((positionals (parser :positionals)))
          (if (and (= (length positionals) 1) (pr-number-string? (car positionals)))
            (car positionals)
            #f
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    (define (run-cmd cmd)
      (newline)
      (display "-->")
      (newline)
      (display "run: ")
      (display cmd)
      (newline)
      (run cmd)
    ) ;define

    (define (current-branch)
      (let-values (((out err code)
                    (run-values (list 'git "branch" "--show-current") :stdout 'capture)
                   ) ;
                  ) ;
        (if (= code 0) (string-trim-both out) #f)
      ) ;let-values
    ) ;define

    (define (run-pr num)
      (let ((remote-ref (string-append "pull/" num "/head"))
            (local-branch (string-append "pr_" num))
            (remote (pr-remote-url))
           ) ;
        (if remote
          (begin
            (display "remote: ")
            (display remote)
            (newline)
            (when (= (run-cmd (string-append "git show-ref --verify --quiet refs/heads/" local-branch)
                     ) ;run-cmd
                    0
                  ) ;=
              (let ((cur (current-branch)))
                (when (and cur (string=? cur local-branch))
                  (run-cmd "git switch --detach")
                ) ;when
                (display (string-append "Deleting existing local branch " local-branch))
                (newline)
                (run-cmd (string-append "git branch -D " local-branch))
              ) ;let
            ) ;when
            (if (= (run-cmd (string-append "git fetch --force " remote " " remote-ref ":" local-branch)
                   ) ;run-cmd
                  0
                ) ;=
              (begin
                (run-cmd (string-append "git switch " local-branch))
                (display (string-append "Now on " local-branch " (PR #" num ")"))
                (newline)
                0
              ) ;begin
              (begin
                (display (string-append "PR #" num " not found on remote"))
                (newline)
                1
              ) ;begin
            ) ;if
          ) ;begin
          (begin
            (display "Error: no origin remote found in this repository")
            (newline)
            1
          ) ;begin
        ) ;if
      ) ;let
    ) ;define

    (define (main)
      (let ((num (parse-pr-args (command-line))))
        (if num
          (run-pr num)
          (begin
            (display "Usage: gf pr NUM" (current-error-port))
            (newline (current-error-port))
            1
          ) ;begin
        ) ;if
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
