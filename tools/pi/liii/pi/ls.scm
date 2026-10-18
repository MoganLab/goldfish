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

(define-library (liii pi ls)
  (import (scheme base)
    (scheme char)
    (liii string)
    (liii os)
    (liii path)
    (liii sort)
    (liii pi read)
    (liii pi truncate)
  ) ;import
  (export pi-ls-schema pi-execute-ls)
  (begin

    (define (pi-ls-schema)
      (list (cons "type" "object")
        (cons "properties"
          (list (cons "path"
                  (list (cons "type" "string")
                    (cons "description" "Directory to list (default: current directory)")
                  ) ;list
                ) ;cons
            (cons "limit"
              (list (cons "type" "number")
                (cons "description" "Maximum number of entries to return (default: 500)")
              ) ;list
            ) ;cons
          ) ;list
        ) ;cons
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

    (define (%format-entries entries abs-path effective-limit)
      (let loop
        ((lst entries) (count 0) (acc '()))
        (cond ((null? lst) (values (reverse acc) #f))
              ((>= count effective-limit) (values (reverse acc) #t))
              (else
                (let* ((entry (car lst))
                       (full-p (path-join (path abs-path) (path entry)))
                       (suffix (if (path-dir? full-p) "/" ""))
                      ) ;
                  (loop (cdr lst) (+ count 1) (cons (string-append entry suffix) acc))
                ) ;let*
              ) ;else
        ) ;cond
      ) ;let
    ) ;define

    (define (%build-notices limit-reached? effective-limit truncated?)
      (let* ((n1
               (if limit-reached?
                 (list (string-append (number->string effective-limit)
                         " entries limit reached. Use limit="
                         (number->string (* effective-limit 2))
                         " for more"
                       ) ;string-append
                 ) ;list
                 '()
               ) ;if
             ) ;n1
             (notices (if truncated? (append n1 (list "50.0KB limit reached")) n1))
            ) ;
        (if (null? notices) "" (string-append "\n\n[" (string-join notices ". ") "]"))
      ) ;let*
    ) ;define

    (define (pi-execute-ls args . opts)
      (let* ((timed-out? (%keyword-value :timed-out? opts #f))
             (cwd (%keyword-value :cwd opts (getcwd)))
             (raw-path (pi-arg-ref args "path" "."))
             (limit (pi-arg-ref args "limit" 500))
             (effective-limit (if (number? limit) limit 500))
            ) ;
        (when timed-out?
          (error 'error "Command timed out")
        ) ;when
        (let ((abs-path (pi-resolve-path raw-path cwd)))
          (unless (path-exists? (path abs-path))
            (error 'error (string-append "Path not found: " abs-path))
          ) ;unless
          (unless (path-dir? (path abs-path))
            (error 'error (string-append "Not a directory: " abs-path))
          ) ;unless
          (let* ((raw-entries (vector->list (listdir abs-path)))
                 (sorted (list-sort string-ci<? raw-entries))
                ) ;
            (if (null? sorted)
              "(empty directory)"
              (let-values (((formatted limit-reached?) (%format-entries sorted abs-path effective-limit)))
                (let* ((raw-output (string-join formatted "\n"))
                       (trunc (pi-truncate-head raw-output 999999 PI_DEFAULT_MAX_BYTES))
                       (content (pi-trunc-result-content trunc))
                       (truncated? (pi-trunc-result-truncated trunc))
                       (notices (%build-notices limit-reached? effective-limit truncated?))
                      ) ;
                  (string-append content notices)
                ) ;let*
              ) ;let-values
            ) ;if
          ) ;let*
        ) ;let
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
