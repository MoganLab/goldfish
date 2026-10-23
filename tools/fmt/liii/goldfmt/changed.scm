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

(define-library (liii goldfmt changed)
  (import (liii base)
    (liii path)
    (liii string)
    (liii list)
    (liii json)
    (liii goldfmt lang)
    (liii goldfmt config)
    (liii goldfmt args)
    (liii goldfmt repo)
    (liii goldtool changed)
  ) ;import
  (export all-registered-extensions group-files-by-lang format-changed-since)
  (begin

    (define (ensure-scheme-handler!)
      (unless (lang-for-extension ".scm")
        (eval '(import (liii goldfmt scheme)) (rootlet))
      ) ;unless
    ) ;define

    (define (scheme-handler-of)
      (ensure-scheme-handler!)
      (lang-for-extension ".scm")
    ) ;define

    (define (all-registered-extensions)
      (ensure-scheme-handler!)
      (let loop
        ((handlers (lang-list)) (acc '()))
        (if (null? handlers)
          acc
          (loop (cdr handlers) (append (lang-extensions (car handlers)) acc))
        ) ;if
      ) ;let
    ) ;define

    (define (add-file-to-group groups file handler)
      (let ((name (lang-name handler)))
        (let loop
          ((gs groups) (acc '()) (found #f))
          (cond
           ((null? gs)
            (if found (reverse acc) (reverse (cons (cons name (list file)) acc)))
           ) ;
           ((and (not found) (eq? (caar gs) name))
            (loop (cdr gs) (cons (cons name (cons file (cdar gs))) acc) #t)
           ) ;
           (else (loop (cdr gs) (cons (car gs) acc) found))
          ) ;cond
        ) ;let
      ) ;let
    ) ;define

    (define (group-files-by-lang files)
      (ensure-scheme-handler!)
      (let loop
        ((fs files) (groups '()))
        (if (null? fs)
          groups
          (let* ((file (car fs))
                 (ext (path-suffix (path file)))
                 (handler (or (lang-for-extension ext) (scheme-handler-of)))
                ) ;
            (loop (cdr fs) (add-file-to-group groups file handler))
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    (define (stats-failed stats)
      (if (> (length stats) 3) (list-ref stats 3) 0)
    ) ;define

    (define (format-changed-group-loop gs cfg total updated cached failed)
      (if (null? gs)
        (begin
          (display
            (string-append "Total files formatted: "
              (number->string total)
              ", Files updated: "
              (number->string updated)
              ", Files cached: "
              (number->string cached)
              (if (> failed 0) (string-append ", Files failed: " (number->string failed)) "")
            ) ;string-append
          ) ;display
          (newline)
          (if (> failed 0) (exit 1) #t)
        ) ;begin
        (let* ((g (car gs))
               (handler (lang-for-name (car g)))
               (format-files-fn (lang-ref handler 'format-files))
               (stats (format-files-fn (cdr g) cfg))
              ) ;
          (display (string-append "=== Formatting " (lang-label handler) " files ==="))
          (newline)
          (flush-output-port (current-output-port))
          (format-changed-group-loop (cdr gs)
            cfg
            (+ total (car stats))
            (+ updated (cadr stats))
            (+ cached (caddr stats))
            (+ failed (stats-failed stats))
          ) ;format-changed-group-loop
        ) ;let*
      ) ;if
    ) ;define

    (define (format-changed-since since path-str extensions excludes dry-run)
      (if dry-run
        (begin
          (display "错误: --dry-run 选项与 --changed-since 不能同时使用")
          (newline)
          (exit 1)
        ) ;begin
        (let* ((scope (if (string=? path-str "") #f path-str))
               (cfg
                 (or (catch #t (lambda () (load-fmt-config)) (lambda (type info) #f))
                   (string->json "{}")
                 ) ;or
               ) ;cfg
               (dummy (check-goldfish-version cfg))
               (files (if scope
                        (changed-existing-files-since since scope)
                        (changed-existing-files-since since)
                      ) ;if
               ) ;files
               (filtered
                 (filter
                   (lambda (f)
                     (and (file-extension-match? f extensions) (not (file-excluded? f excludes)))
                   ) ;lambda
                   files
                 ) ;filter
               ) ;filtered
              ) ;
          (if (null? filtered)
            (begin
              (display (string-append "No changed files since " since))
              (newline)
              #t
            ) ;begin
            (format-changed-group-loop (group-files-by-lang filtered) cfg 0 0 0 0)
          ) ;if
        ) ;let*
      ) ;if
    ) ;define

  ) ;begin
) ;define-library
