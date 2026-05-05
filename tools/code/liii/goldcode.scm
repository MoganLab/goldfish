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

(define-library (liii goldcode)
  (import (scheme base)
    (scheme file)
    (scheme write)
    (liii os)
    (liii path)
    (liii string)
    (liii sys)
  ) ;import
  (export main run-goldcode)
  (begin

    (define (stderr-line message)
      (display message (current-error-port))
      (newline (current-error-port))
    ) ;define

    (define (get-current-branch)
      (let ((tmp-file (path->string (path-join (path-temp-dir) (path "gf-branch.txt")))))
        (os-call (string-append "git rev-parse --abbrev-ref HEAD > \"" tmp-file "\""))
        (let ((result (if (file-exists? tmp-file) (string-trim-both (path-read-text tmp-file)) "")
              ) ;result
             ) ;
          (when (file-exists? tmp-file)
            (remove tmp-file)
          ) ;when
          result
        ) ;let
      ) ;let
    ) ;define

    (define (check-main-branch)
      (let ((branch (get-current-branch)))
        (string=? branch "main")
      ) ;let
    ) ;define

    (define (has-origin-remote)
      (= (os-call "git remote get-url origin") 0)
    ) ;define

    (define (chmod-command target)
      (if (os-windows?) "" (string-append "chmod 755 \"" target "\""))
    ) ;define

    (define (sync-pre-commit-hook)
      (let ((source-hook "hooks/pre-commit") (target-hook ".git/hooks/pre-commit"))
        (if (not (file-exists? source-hook))
          (begin
            (display "Source hook ")
            (display source-hook)
            (display " does not exist, skipping...")
            (newline)
          ) ;begin
          (let ((sync-needed (if (not (file-exists? target-hook))
                               #t
                               (not (string=? (path-read-text source-hook) (path-read-text target-hook)))
                             ) ;if
                ) ;sync-needed
               ) ;
            (when sync-needed
              (display "Syncing pre-commit hook...")
              (newline)
              (path-copy source-hook target-hook)
              (let ((chmod-cmd (chmod-command target-hook)))
                (when (and (not (os-windows?)) (> (string-length chmod-cmd) 0))
                  (os-call chmod-cmd)
                ) ;when
              ) ;let
              (display "Pre-commit hook synchronized successfully.")
              (newline)
            ) ;when
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (pull-latest-code)
      (if (check-main-branch)
        (begin
          (if (has-origin-remote)
            (begin
              (display "Pulling latest code from main branch...")
              (newline)
              (os-call "git pull origin main")
            ) ;begin
            (begin
              (display "Adding origin git@gitee.com:XmacsLabs/goldfish.git...")
              (newline)
              (os-call "git remote add origin git@gitee.com:XmacsLabs/goldfish.git")
              (display "Pulling latest code from main branch...")
              (newline)
              (os-call "git pull origin main")
              (display "Removing temporary origin...")
              (newline)
              (os-call "git remote remove origin")
            ) ;begin
          ) ;if
        ) ;begin
        (begin
          (display "Not on main branch, skipping git pull from origin/main...")
          (newline)
        ) ;begin
      ) ;if
    ) ;define

    (define (launch-claude)
      (display "Launching Claude Code...")
      (newline)
      (g_system "claude --dangerously-skip-permissions")
    ) ;define

    (define (run-goldcode)
      ;; Sync pre-commit hook
      (sync-pre-commit-hook)

      ;; Check if we should verify main branch (only when extra arguments are provided)
      (let ((args (argv)))
        (if (> (length args) 2)
          (if (not (check-main-branch))
            (let ((branch (get-current-branch)))
              (stderr-line (string-append "Error: You are not on the main branch. Current branch: " branch)
              ) ;stderr-line
              (stderr-line "Please switch to main branch first: git checkout main")
              1
            ) ;let
            (begin
              (display "Already on main branch.")
              (newline)
              (pull-latest-code)
              (launch-claude)
              0
            ) ;begin
          ) ;if
          (begin
            (display "No branch check argument provided, skipping main branch check...")
            (newline)
            (pull-latest-code)
            (launch-claude)
            0
          ) ;begin
        ) ;if
      ) ;let
    ) ;define

    (define (main)
      (run-goldcode)
    ) ;define

  ) ;begin
) ;define-library
