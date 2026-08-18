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

    (define (git-out . args)
      (let-values (((out err code) (run-values (cons 'git args) :stdout 'capture)))
        (if (and (= code 0) (> (string-length out) 0)) (string-trim-both out) #f)
      ) ;let-values
    ) ;define

    (define (pr-remote-url . opts)
      (let ((remote (if (null? opts) "origin" (car opts))))
        (git-out "remote" "get-url" remote)
      ) ;let
    ) ;define

    (define (pr-number-string? value)
      (and (string? value)
        (not (string-null? value))
        (string-every char-numeric? value)
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

    (define (run-cmd args)
      (display "\n-->\nrun: ")
      (display (string-join (cons "git" args) " "))
      (newline)
      (run (cons 'git args))
    ) ;define

    (define (current-branch)
      (git-out "branch" "--show-current")
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
            (when (= (run-cmd (list "show-ref"
                                "--verify"
                                "--quiet"
                                (string-append "refs/heads/" local-branch)
                              ) ;list
                     ) ;run-cmd
                    0
                  ) ;=
              (let ((cur (current-branch)))
                (when (and cur (string=? cur local-branch))
                  (run-cmd '("switch" "--detach"))
                ) ;when
                (display (string-append "Deleting existing local branch " local-branch))
                (newline)
                (run-cmd (list "branch" "-D" local-branch))
              ) ;let
            ) ;when
            (if (= (run-cmd (list "fetch" "--force" remote (string-append remote-ref ":" local-branch))
                   ) ;run-cmd
                  0
                ) ;=
              (begin
                (run-cmd (list "switch" local-branch))
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
