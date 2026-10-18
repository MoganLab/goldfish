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

(define-library (liii pi find)
  (import (scheme base)
    (liii string)
    (liii list)
    (liii os)
    (liii path)
    (liii subprocess)
    (liii either)
    (liii pi read)
    (liii pi truncate)
  ) ;import
  (export pi-find-schema pi-execute-find)
  (begin

    (define (pi-find-schema)
      (list (cons "type" "object")
        (cons "properties"
          (list (cons "pattern"
                  (list (cons "type" "string")
                    (cons "description"
                      "Glob pattern to match files, e.g. '*.ts', '**/*.json', or 'src/**/*.spec.ts'"
                    ) ;cons
                  ) ;list
                ) ;cons
            (cons "path"
              (list (cons "type" "string")
                (cons "description" "Directory to search in (default: current directory)")
              ) ;list
            ) ;cons
            (cons "limit"
              (list (cons "type" "number")
                (cons "description" "Maximum number of results (default: 1000)")
              ) ;list
            ) ;cons
          ) ;list
        ) ;cons
        (cons "required" #("pattern"))
      ) ;list
    ) ;define

    (define (%keyword-value key opts default)
      (let loop
        ((rest opts))
        (cond ((null? rest) default)
              ((and (pair? (cdr rest)) (eq? (car rest) key)) (cadr rest))
              ((null? (cdr rest)) default)
              (else (loop (cddr rest)))
        ) ;cond
      ) ;let
    ) ;define

    (define (%glob-match relpath pattern)
      (let* ((p (path relpath))
             (clean-pat (if (string-starts? pattern "**/")
                          (substring pattern 3 (string-length pattern))
                          pattern
                        ) ;if
             ) ;clean-pat
            ) ;
        (or (path-match p pattern)
          (path-match p clean-pat)
          (path-match (path (path-name p)) clean-pat)
        ) ;or
      ) ;let*
    ) ;define

    (define (%git-files-in-dir abs-dir timeout)
      (let ((res (run-either (list "git" "ls-files" "--cached" "--others" "--exclude-standard")
                   :cwd     abs-dir
                   :stdout  'capture
                   :stderr  'discard
                   :timeout timeout
                 ) ;run-either
            ) ;res
           ) ;
        (if (either-right? res)
          (let ((lines (string-split (to-right res) "\n")))
            (filter (lambda (s) (not (string-null? s))) lines)
          ) ;let
          #f
        ) ;if
      ) ;let
    ) ;define

    (define (%find-all-files abs-dir timeout)
      (let ((git-files (%git-files-in-dir abs-dir timeout)))
        (if git-files
          git-files
          (let ((paths (path-rglob (path abs-dir) "*")))
            (map
              (lambda (p)
                (let ((rel (path-relative-to p (path abs-dir))))
                  (if rel (path->string rel) (path->string p))
                ) ;let
              ) ;lambda
              paths
            ) ;map
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (%build-find-notices limit-reached? effective-limit truncated?)
      (let* ((n1
               (if limit-reached?
                 (list (string-append (number->string effective-limit) " results limit reached"))
                 '()
               ) ;if
             ) ;n1
             (notices (if truncated? (append n1 (list "50.0KB limit reached")) n1))
            ) ;
        (if (null? notices) "" (string-append "\n\n[" (string-join notices ". ") "]"))
      ) ;let*
    ) ;define

    (define (pi-execute-find args . opts)
      (let* ((timed-out? (%keyword-value :timed-out? opts #f))
             (cwd (%keyword-value :cwd opts (getcwd)))
             (timeout (%keyword-value :timeout opts 120))
             (pattern (pi-arg-ref args "pattern"))
             (raw-path (pi-arg-ref args "path" "."))
             (limit (pi-arg-ref args "limit" 1000))
             (effective-limit (if (number? limit) limit 1000))
            ) ;
        (when timed-out?
          (error 'error "Command timed out")
        ) ;when
        (unless (string? pattern)
          (error 'error "pattern must be a string")
        ) ;unless
        (let ((abs-path (pi-resolve-path raw-path cwd)))
          (unless (path-exists? (path abs-path))
            (error 'error (string-append "Path not found: " abs-path))
          ) ;unless
          (let* ((all-files (%find-all-files abs-path timeout))
                 (matched (filter (lambda (f) (%glob-match f pattern)) all-files))
                ) ;
            (if (null? matched)
              "No files found matching pattern"
              (let* ((limit-reached? (>= (length matched) effective-limit))
                     (selected (if limit-reached? (take matched effective-limit) matched))
                     (raw-output (string-join selected "\n"))
                     (trunc (pi-truncate-head raw-output 999999 PI_DEFAULT_MAX_BYTES))
                     (content (pi-trunc-result-content trunc))
                     (truncated? (pi-trunc-result-truncated trunc))
                     (notices (%build-find-notices limit-reached? effective-limit truncated?))
                    ) ;
                (string-append content notices)
              ) ;let*
            ) ;if
          ) ;let*
        ) ;let
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
