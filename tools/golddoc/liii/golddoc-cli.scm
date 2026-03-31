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
        (display "  gf doc ORG/LIB FUNC    (not implemented yet)" port) (newline port)
        (display "  gf doc FUNC            (not implemented yet)" port) (newline port)
      ) ;let
    ) ;define

    (define (run-golddoc)
      (let ((parsed (parse-doc-args (argv))))
        (case (car parsed)
          ((library)
           (let* ((query (cadr parsed))
                  (parts (parse-library-query query))
                  (group (and parts (car parts)))
                  (doc-path (library-doc-path query)))
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
               ((not (find-visible-library-root query))
                (stderr-line (string-append "Error: library not found in *load-path*: " query))
                1
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
           (stderr-line "Error: function documentation queries are not implemented yet.")
           1
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
