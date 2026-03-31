;
; Copyright (C) 2026 The Goldfish Scheme Authors
;
; Licensed under the Apache License, Version 2.0 (the "License");
; you may not use this file except in compliance with the License.
; You may obtain a copy of the License at
;
; http://www.apache.org/licenses/LICENSE-2.0
;
; Unless required by applicable law or agreed to in writing, software
; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
; License for the specific language governing permissions and limitations
; under the License.
;

(define-library (liii golddoc-cli)
  (import (scheme base)
          (liii golddoc-args)
          (liii golddoc-function)
          (liii golddoc-index)
          (liii golddoc-index-build)
          (liii golddoc-library)
          (liii path)
          (liii sys)
  ) ;import
  (export run-golddoc)
  (begin

    (define (stderr-line message)
      (display message (current-error-port))
      (newline (current-error-port))
    ) ;define

    (define (display-usage)
      (let ((port (current-error-port)))
        (display "Usage:" port) (newline port)
        (display "  gf doc ORG/LIB" port) (newline port)
        (display "  gf doc ORG/LIB FUNC" port) (newline port)
        (display "  gf doc FUNC" port) (newline port)
        (display "  gf doc --build-json" port) (newline port)
      ) ;let
    ) ;define

    (define (display-library-choices function-name library-queries)
      (let ((port (current-error-port)))
        (display (string-append "Function is implemented in multiple visible libraries: " function-name) port)
        (newline port)
        (for-each
          (lambda (library-query)
            (display "  " port)
            (display library-query port)
            (newline port)
          ) ;lambda
          library-queries
        ) ;for-each
      ) ;let
    ) ;define

    (define (run-function-query function-name)
      (let ((library-queries (visible-libraries-for-function function-name)))
        (cond
          ((null? library-queries)
           (stderr-line (string-append "Error: function not found in *load-path*: " function-name))
           1
          ) ;
          ((null? (cdr library-queries))
           (let ((doc-path (function-doc-path (car library-queries) function-name)))
             (if doc-path
                 (begin
                   (display (path-read-text doc-path))
                   0
                 ) ;begin
                 (begin
                   (stderr-line (string-append "Error: documentation file not found for function: " function-name))
                   1
                 ) ;begin
             ) ;if
           ) ;let
          ) ;
          (else
           (display-library-choices function-name library-queries)
           1
          ) ;else
        ) ;cond
      ) ;let
    ) ;define

    (define (run-golddoc)
      (let ((parsed (parse-doc-args (argv))))
        (case (car parsed)
          ((build-json)
           (let ((built-paths (build-function-indexes!)))
             (if (null? built-paths)
                 (begin
                   (stderr-line "Error: no buildable tests roots found in *load-path*.")
                   1
                 ) ;begin
                 (begin
                   (for-each
                     (lambda (built-path)
                       (display "Built function index: ")
                       (display built-path)
                       (newline)
                     ) ;lambda
                     built-paths
                   ) ;for-each
                   0
                 ) ;begin
             ) ;if
           ) ;let
          ) ;
          ((library)
           (let* ((query (cadr parsed))
                  (parts (parse-library-query query))
                  (group (and parts (car parts)))
                  (doc-path (library-doc-path query))
                  (visible-library-root (and parts
                                             (find-visible-library-root query))))
             (cond
               ((not parts)
                (display-usage)
                1
               ) ;
               ((excluded-test-group? group)
                (stderr-line (string-append "Error: documentation for tests/" group " is not supported yet."))
                1
               ) ;
               (doc-path
                (display (path-read-text doc-path))
                0
               ) ;doc-path
               ((not visible-library-root)
                (let ((fallback-libraries (visible-libraries-for-function query)))
                  (if (null? fallback-libraries)
                      (begin
                        (stderr-line (string-append "Error: library not found in *load-path*: " query))
                        1
                      ) ;begin
                      (run-function-query query)
                  ) ;if
                ) ;let
               ) ;
               (else
                (stderr-line (string-append "Error: documentation file not found for library: " query))
                1
               ) ;else
             ) ;cond
           ) ;let*
          ) ;
          ((library-function)
           (let* ((library-query (cadr parsed))
                  (exported-name (caddr parsed))
                  (parts (parse-library-query library-query))
                  (group (and parts (car parts)))
                  (doc-path (function-doc-path library-query exported-name)))
             (cond
               ((not parts)
                (display-usage)
                1
               ) ;
               ((excluded-test-group? group)
                (stderr-line (string-append "Error: documentation for tests/" group " is not supported yet."))
                1
               ) ;
               (doc-path
                (display (path-read-text doc-path))
                0
               ) ;doc-path
               ((not (find-visible-library-root library-query))
                (stderr-line (string-append "Error: library not found in *load-path*: " library-query))
                1
               ) ;
               (else
                (stderr-line (string-append "Error: documentation file not found for function: "
                                            exported-name
                                            " in library: "
                                            library-query)
                ) ;stderr-line
                1
               ) ;else
             ) ;cond
          ) ;let*
          ) ;
          ((function)
           (run-function-query (cadr parsed))
          ) ;
          (else
           (display-usage)
           1
          ) ;else
        ) ;case
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
