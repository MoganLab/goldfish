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

(define-library (liii goldtool-changed)
  (import (scheme base) (liii list) (liii os) (liii path) (liii string))
  (export changed-files-since
    changed-existing-files-since
    changed-scheme-files-since
  ) ;export
  (begin

    (define (posix-shell-quote value)
      (let ((text (if (string? value) value (path->string value))))
        (let loop
          ((chars (string->list text)) (pieces '("'")))
          (if (null? chars)
            (apply string-append (reverse (cons "'" pieces)))
            (loop (cdr chars)
              (cons (if (char=? (car chars) (integer->char 39)) "'\\''" (string (car chars)))
                pieces
              ) ;cons
            ) ;loop
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    (define (windows-shell-quote value)
      (let ((text (if (string? value) value (path->string value))))
        (string-append "\"" (string-replace text "\"" "\\\"") "\"")
      ) ;let
    ) ;define

    (define (shell-quote value)
      (if (os-windows?) (windows-shell-quote value) (posix-shell-quote value))
    ) ;define

    (define (changed-output-path)
      (path->string (path-join (path-temp-dir)
                      (string-append "goldfish-changed-" (number->string (getpid)) ".txt")
                    ) ;path-join
      ) ;path->string
    ) ;define

    (define (run-shell command)
      (os-call (if (os-windows?) command (string-append "sh -c " (posix-shell-quote command)))
      ) ;os-call
    ) ;define

    (define (non-empty-lines text)
      (filter (lambda (line) (not (string-null? line))) (string-split text "\n"))
    ) ;define

    (define (changed-files-since since . maybe-path)
      (let* ((output-path (changed-output-path))
             (scope (if (null? maybe-path) #f (car maybe-path)))
             (scope-part (if (and scope (not (string=? scope "")))
                           (string-append " " (shell-quote scope))
                           ""
                         ) ;if
             ) ;scope-part
             (command (string-append "git diff --name-only --relative "
                        (shell-quote since)
                        " --"
                        scope-part
                        " > "
                        (shell-quote output-path)
                      ) ;string-append
             ) ;command
            ) ;
        (path-unlink output-path #t)
        (let ((status (run-shell command)))
          (if (zero? status)
            (let ((files (non-empty-lines (path-read-text output-path))))
              (path-unlink output-path #t)
              files
            ) ;let
            (begin
              (path-unlink output-path #t)
              (error "Failed to compute changed files since" since)
            ) ;begin
          ) ;if
        ) ;let
      ) ;let*
    ) ;define

    (define (changed-existing-files-since since . maybe-path)
      (filter path-file?
        (if (null? maybe-path)
          (changed-files-since since)
          (changed-files-since since (car maybe-path))
        ) ;if
      ) ;filter
    ) ;define

    (define (changed-scheme-files-since since . maybe-path)
      (let* ((has-extension-param? (and (not (null? maybe-path))
                                        (not (null? (cdr maybe-path)))))
             (extensions (if has-extension-param? (cadr maybe-path) '(".scm")))
             (path (if has-extension-param? (car maybe-path)
                     (if (null? maybe-path) #f (car maybe-path)))))
        (filter (lambda (file)
                  (and (path-file? file)
                    (let loop ((exts extensions))
                      (if (null? exts)
                        #f
                        (if (string-ends? file (car exts))
                          #t
                          (loop (cdr exts))
                        ) ;if
                      ) ;if
                    ) ;let
                  ) ;and
                ) ;lambda
          (if path
            (changed-files-since since path)
            (changed-files-since since)
          ) ;if
        ) ;filter
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
